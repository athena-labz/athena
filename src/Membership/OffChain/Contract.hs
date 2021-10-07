{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Membership.OffChain.Contract where

import Control.Monad (forever)
import qualified Data.Map as Map
import Data.Monoid as M (Last (Last), Monoid (mconcat))
import Data.Text (Text, pack)
import Ledger
  ( ChainIndexTxOut,
    Datum (Datum),
    PubKeyHash,
    Redeemer (Redeemer),
    TxOut (txOutValue),
    TxOutRef,
    TxOutTx (..),
    ValidatorHash,
    lookupDatum,
    pubKeyHash,
    toTxOut,
    txId,
  )
import Ledger.Constraints as Constraints
  ( ScriptLookups,
    TxConstraints,
    mustBeSignedBy,
    mustPayToOtherScript,
    mustPayToPubKey,
    mustPayToTheScript,
    mustSpendScriptOutput,
    otherScript,
    typedValidatorLookups,
    unspentOutputs,
  )
import Ledger.Typed.Scripts (ValidatorTypes (..))
import Ledger.Value
  ( AssetClass (..),
    CurrencySymbol,
    Value,
    assetClass,
    assetClassValue,
    assetClassValueOf,
    geq,
    singleton,
  )
import Membership.Account
import Membership.Contract
import Membership.OffChain.Utils
import Membership.OnChain.Account
import Membership.OnChain.Contract
import Membership.PlatformSettings
import Membership.Service
import Membership.Signature (findSignatories, findSignatory, makeSigToken)
import Plutus.Contract as Contract
  ( Contract,
    Endpoint,
    Promise (awaitPromise),
    awaitTxConfirmed,
    currentTime,
    endpoint,
    handleError,
    logError,
    logInfo,
    mapError,
    ownPubKey,
    select,
    submitTxConstraintsWith,
    tell,
    type (.\/),
  )
import Plutus.Contracts.Currency as Currency
  ( CurrencyError,
    OneShotCurrency,
    currencySymbol,
    mintContract,
    mintedValue,
  )
import Plutus.V2.Ledger.Api (CurrencySymbol (CurrencySymbol))
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AM
import PlutusTx.Prelude
  ( AdditiveGroup ((-)),
    Bool,
    BuiltinByteString,
    Eq ((==)),
    Integer,
    Maybe (..),
    filter,
    fst,
    length,
    map,
    mempty,
    negate,
    otherwise,
    return,
    traceError,
    ($),
    (*),
    (++),
    (.),
    (/=),
    (<),
    (<$>),
    (>),
    (||),
  )
import qualified PlutusTx.Ratio as R
import Wallet.Emulator.Wallet ()
import Prelude (Semigroup (..), Show (..), String, uncurry)

-- Based on the given initial contract datum, createContract mints a new NFT,
-- and transfers it to the platform contract script address, creating a new UTxO
-- this UTxO also receives the user's SIG token and the trust amount, which serves
-- as a collateral
createContract ::
  AccountSettings ->
  ContractDatum ->
  Contract (Last AssetClass) s Text ()
createContract accountSettings contractDatum = do
  -- The public key hash of the user who is trying to create a new contract
  pkh <- pubKeyHash <$> Contract.ownPubKey

  -- The essential information needed to create a contract
  maybeAccountOffChainEssentials <-
    getAccountOffChainEssentials accountSettings pkh

  -- Mints a NFT and holds it's reference in a variable
  -- This NFT will serve as a way to uniquely identify our contract
  -- and make sure we are always consuming the write one
  contractNFT <-
    mapError
      (pack . show)
      ( mintContract pkh [("contract-nft", 1)] ::
          Contract w s CurrencyError OneShotCurrency
      )

  -- Verifies if we were able or not to get the account important information.
  -- If so, completes the transaction. Otherwise, logs an error and leave
  case maybeAccountOffChainEssentials of
    Just aoe -> do
      -- Submit transaction
      ledgerTx <- submitTxConstraintsWith @AccountType lookups tx

      -- Update the last value to our currenct NFT created
      tell $ Last $ Just contractNFTAssetClass

      -- Wait until transaction is confirmed
      awaitTxConfirmed $ txId ledgerTx

      logInfo @String $
        "Create Contract - "
          ++ "Contract succefully created "
          ++ show contractNFTAssetClass
      where
        -- The currency symbol for the SIG tokens
        sigSymbol :: CurrencySymbol
        sigSymbol = asSignatureSymbol accountSettings

        -- The platform settings derived from account settings
        platformSettings :: PlatformSettings
        platformSettings = asPlatformSettings accountSettings

        -- The reference to the account UTxO from our user
        accountReference :: TxOutRef
        accountReference = aoeAccountReference aoe

        -- The datum fron the account we are consuming
        accountDatum :: AccountDatum
        accountDatum = aoeAccountDatum aoe

        -- A chain index version of the account script output which is
        -- being consumed
        accountChainIndexTxOut :: ChainIndexTxOut
        accountChainIndexTxOut = fst (aoeAccountOutTx aoe)

        -- The important information about the account output which is
        -- being consumed
        accountOutput :: TxOut
        accountOutput = toTxOut accountChainIndexTxOut

        -- The currency symbol from the NFT we just minted
        contractNFTSymbol :: CurrencySymbol
        contractNFTSymbol = Currency.currencySymbol contractNFT

        -- The asset class from the NFT we just minted
        contractNFTAssetClass :: AssetClass
        contractNFTAssetClass = assetClass contractNFTSymbol "contract-nft"

        -- The script validator hashes from the platform accounts and contracts
        accValHash, contrValHash :: ValidatorHash
        accValHash = accountValidatorHash accountSettings
        contrValHash = asContractValidatorHash accountSettings

        -- The account datum we will give to the newly created account UTxO
        -- It should increase our CAS score, since we are using the platform
        -- and should register the newly created contract and it's NFT
        newAccountDatum :: AccountDatum
        newAccountDatum =
          addContract
            (contractCreationCAS (psCASMap platformSettings) accountDatum)
            contrValHash
            contractNFTAssetClass

        -- Some values that we are going send from the account or the user wallet
        -- to the contract
        sigValue, txFeeValue, trustValue :: Value
        sigValue = singleton sigSymbol (makeSigToken pkh accValHash) 1
        txFeeValue =
          assetClassValue
            (psToken platformSettings)
            (psTxFee platformSettings)
        trustValue =
          assetClassValue
            (psToken platformSettings)
            (sTrust $ cdService contractDatum)

        -- The expected resulting values in each script
        contractValue, accountValue :: Value
        contractValue =
          sigValue <> Currency.mintedValue contractNFT <> trustValue
        accountValue = txOutValue accountOutput <> txFeeValue <> negate sigValue

        lookups :: ScriptLookups AccountType
        lookups =
          accountLookups accountSettings accountReference accountChainIndexTxOut

        tx :: TxConstraints (RedeemerType AccountType) (DatumType AccountType)
        tx =
          Constraints.mustBeSignedBy pkh
            <> spendAccount accountReference ACreateContract
            <> Constraints.mustPayToTheScript newAccountDatum accountValue
            <> Constraints.mustPayToOtherScript
              contrValHash
              (Datum $ PlutusTx.toBuiltinData contractDatum)
              contractValue
    Nothing -> logError @String "Create Contract - Contract Essentials failed"

-- Based on the role we intend to have and an asset class that represent the
-- nft from the contract the user want's to sign, adds the user's SIG token
-- to the contract, together with the necessary value (trust and price maybe),
-- changing the datum according to our role
signContract :: Role -> AccountSettings -> AssetClass -> Contract w s Text ()
signContract userRole accountSettings contractNFT = do
  -- The signer public key hash
  pkh <- pubKeyHash <$> Contract.ownPubKey

  -- Tries to get the account off-chain essentials
  maybeAccountOffChainEssentials <-
    getAccountOffChainEssentials accountSettings pkh

  -- Tries to get the contract off-chain essentials
  maybeContractOffChainEssentials <-
    getContractOffChainEssentials accountSettings contractNFT

  -- Verifies if both account and contract important information were gathered.
  -- If they all were, completes the transaction. Otherwise, logs an error and
  -- leaves.
  case (maybeAccountOffChainEssentials, maybeContractOffChainEssentials) of
    (Just aoe, Just coe) -> do
      -- Submit transaction
      ledgerTx <- submitTxConstraintsWith @ContractType lookups tx

      -- Wait until transaction is confirmed
      awaitTxConfirmed $ txId ledgerTx

      logInfo @String $
        "Sign Contract - Successfully signed contract "
          ++ show contractNFT
          ++ " by user "
          ++ show pkh
      where
        -- The currency symbol used in every SIG token
        sigSymbol :: CurrencySymbol
        sigSymbol = asSignatureSymbol accountSettings

        -- The information needed in all the platform
        platformSettings :: PlatformSettings
        platformSettings = asPlatformSettings accountSettings

        -- The script validator hashes from the platform accounts and contracts
        accValHash, contrValHash :: ValidatorHash
        accValHash = accountValidatorHash accountSettings
        contrValHash = coeContractValidatorHash coe

        -- An account and contract transaction output we are trying to consume
        -- more fitted to the blockchain
        accountChainIndexTxOut, contractChainIndexTxOut :: ChainIndexTxOut
        accountChainIndexTxOut = fst (aoeAccountOutTx aoe)
        contractChainIndexTxOut = fst (coeContractOutTx coe)

        -- An account and contract transaction output we are trying to consume
        -- easy to understand and modify
        accountOut, contractOut :: TxOut
        accountOut = toTxOut accountChainIndexTxOut
        contractOut = toTxOut contractChainIndexTxOut

        -- The data held in the account output we are trying to consume
        accountDatum :: AccountDatum
        accountDatum = aoeAccountDatum aoe

        -- The data held in the contract output we are trying to consume
        contractDatum :: ContractDatum
        contractDatum = coeContractDatum coe

        -- A blockchain reference to both the account and contract output we
        -- are trying to consume
        accountReference, contractReference :: TxOutRef
        accountReference = aoeAccountReference aoe
        contractReference = coeContractReference coe

        -- The information needed to transact with a contract
        contractSett :: ContractSettings
        contractSett = coeContractSettings coe

        -- The values, which will either be sent or consumed from the contract
        -- and account
        sigValue, trustValue, priceValue, txFeesValue :: Value
        sigValue = singleton sigSymbol (makeSigToken pkh accValHash) 1
        trustValue =
          assetClassValue
            (psToken platformSettings)
            (sTrust $ cdService contractDatum)
        priceValue = case sType (cdService contractDatum) of
          OneTime p _
            | userRole == Client -> p
          _ -> mempty
        txFeesValue =
          assetClassValue
            (psToken platformSettings)
            (psTxFee platformSettings)

        -- Account and contract resulting values
        contractValue, accountValue :: Value
        -- In this case, the contract must receive the trust value (a collateral
        -- in case the contract is broken or cancelled), one user's SIG token
        -- and the service price
        contractValue =
          txOutValue contractOut
            <> trustValue
            <> sigValue
            <> priceValue
        -- The account, in the other hand, is losing one SIG token and earning
        -- the transaction fees. Which can not be used by the account owner, but 
        -- instead can be transfered to the "governance script", which should
        -- administrate the platform funds
        accountValue =
          txOutValue accountOut
            <> txFeesValue
            <> negate sigValue

        -- The data that will be given to the newly created contract UTxO
        -- In this case, the user is being added to the contract role map, which
        -- is a data type that maps each user to his role (Publisher, Client
        -- or Mediator). In our case, we can only be the two last ones
        newContractDatum :: ContractDatum
        newContractDatum = addUser pkh userRole contractDatum

        -- The data that will be given to the newly created account UTxO
        -- In this case, the CAS is increased because the user is using the
        -- platform by singing a contract
        newAccountDatum :: AccountDatum
        newAccountDatum =
          signContractCAS
            (psCASMap $ asPlatformSettings accountSettings)
            (addContract accountDatum contrValHash contractNFT)

        lookups :: ScriptLookups ContractType
        lookups =
          Constraints.unspentOutputs
            ( Map.fromList
                [ (accountReference, accountChainIndexTxOut),
                  (contractReference, contractChainIndexTxOut)
                ]
            )
            <> Constraints.otherScript (accountValidator accountSettings)
            <> Constraints.otherScript (contractValidator contractSett)
            <> Constraints.typedValidatorLookups
              (typedContractValidator contractSett)

        tx :: TxConstraints (RedeemerType ContractType) (DatumType ContractType)
        tx =
          Constraints.mustBeSignedBy pkh
            <> Constraints.mustSpendScriptOutput
              accountReference
              (Redeemer $ PlutusTx.toBuiltinData ASign)
            <> Constraints.mustSpendScriptOutput
              contractReference
              (Redeemer $ PlutusTx.toBuiltinData CSign)
            <> Constraints.mustPayToTheScript newContractDatum contractValue
            <> Constraints.mustPayToOtherScript
              accValHash
              (Datum $ PlutusTx.toBuiltinData newAccountDatum)
              accountValue
    _ ->
      logError @String $
        "Sign Contract - "
          ++ "Account or Contract Essentials failed"

-- Allows a user to leave a contract when supposidely couldn't, lowering his
-- CAS score and not giving his trust tokens back or the price he paid for the
-- service. The only thing his account receives is the user's SIG token.
cancelContract :: AccountSettings -> AssetClass -> Contract w s Text ()
cancelContract accountSettings contractNFT = do
  -- The signer public key hash
  pkh <- pubKeyHash <$> Contract.ownPubKey

  -- Tries to get the account off-chain essentials
  maybeAccountOffChainEssentials <-
    getAccountOffChainEssentials accountSettings pkh

  -- Tries to get the contract off-chain essentials
  maybeContractOffChainEssentials <-
    getContractOffChainEssentials accountSettings contractNFT

  -- Verifies if both account and contract important information were gathered.
  -- If they all were, completes the transaction. Otherwise, logs an error and
  -- leaves.
  case (maybeAccountOffChainEssentials, maybeContractOffChainEssentials) of
    (Just aoe, Just coe) -> do
      -- Submit transaction
      ledgerTx <- submitTxConstraintsWith @ContractType lookups tx

      -- Wait until transaction is confirmed
      awaitTxConfirmed $ txId ledgerTx

      logInfo @String $ show (cdRoleMap newContractDatum)

      logInfo @String $
        "Cancel Contract - Successfully canceled contract " ++ show contractNFT
      where
        -- The currency symbol used in every SIG token
        sigSymbol :: CurrencySymbol
        sigSymbol = asSignatureSymbol accountSettings

        -- The information needed in all the platform
        platformSettings :: PlatformSettings
        platformSettings = asPlatformSettings accountSettings

        -- The script validator hashes from the platform accounts and contracts
        accValHash, contrValHash :: ValidatorHash
        accValHash = accountValidatorHash accountSettings
        contrValHash = coeContractValidatorHash coe

        -- A reference to the account and contract UTxO we are trying to consume
        accountReference, contractReference :: TxOutRef
        accountReference = aoeAccountReference aoe
        contractReference = coeContractReference coe

        -- A version of the UTxOs we are consuming easy to be digested by the
        -- blockchain
        accountChainIndexTxOut, contractChainIndexTxOut :: ChainIndexTxOut
        accountChainIndexTxOut = fst (aoeAccountOutTx aoe)
        contractChainIndexTxOut = fst (coeContractOutTx coe)

        -- A version of the UTxOs we are consuming easy to read and
        -- manipulate
        accountOut, contractOut :: TxOut
        accountOut = toTxOut accountChainIndexTxOut
        contractOut = toTxOut contractChainIndexTxOut

        -- The data from the account we are consuming
        accountDatum :: AccountDatum
        accountDatum = aoeAccountDatum aoe

        -- The data from the contract we are consuming
        contractDatum :: ContractDatum
        contractDatum = coeContractDatum coe

        -- The value corresponding to a single SIG token from the user who
        -- invoked this transaction and want's to leave the contract
        sigValue :: Value
        sigValue = singleton sigSymbol (makeSigToken pkh accValHash) 1

        -- The resulting contract and account values
        -- In this case the contract SIG token is simply being transfered to the
        -- account back
        contractValue, accountValue :: Value
        contractValue =
          txOutValue contractOut <> negate sigValue
        accountValue = txOutValue accountOut <> sigValue

        -- The information needed to transact with a contract
        contractSett :: ContractSettings
        contractSett =
          ContractSettings
            { csPlatformSettings = platformSettings,
              csSignatureSymbol = sigSymbol
            }

        -- The data that will be given to the newly created contract UTxO
        -- In this case, the user is being removed from the contract role map
        newContractDatum :: ContractDatum
        newContractDatum = removeUser pkh contractDatum

        -- The data that will be given to the newly created account UTxO
        -- In this case, the contract is being removed from the account register
        -- of contracts and the CAS is being decreased, because the user is
        -- cancelling a contract
        newAccountDatum :: AccountDatum
        newAccountDatum =
          cancelContractCAS
            (psCASMap platformSettings)
            (removeContract accountDatum contrValHash)

        lookups :: ScriptLookups ContractType
        lookups =
          Constraints.unspentOutputs
            ( Map.fromList
                [ (accountReference, accountChainIndexTxOut),
                  (contractReference, contractChainIndexTxOut)
                ]
            )
            <> Constraints.otherScript (accountValidator accountSettings)
            <> Constraints.otherScript (contractValidator contractSett)
            <> Constraints.typedValidatorLookups
              (typedContractValidator contractSett)

        tx :: TxConstraints (RedeemerType ContractType) (DatumType ContractType)
        tx =
          Constraints.mustBeSignedBy pkh
            <> Constraints.mustSpendScriptOutput
              accountReference
              (Redeemer $ PlutusTx.toBuiltinData (AReturn ARTCancel))
            <> Constraints.mustSpendScriptOutput
              contractReference
              (Redeemer $ PlutusTx.toBuiltinData CCancel)
            <> Constraints.mustPayToTheScript
              newContractDatum
              contractValue
            <> Constraints.mustPayToOtherScript
              accValHash
              (Datum $ PlutusTx.toBuiltinData newAccountDatum)
              accountValue
    _ ->
      logError @String $
        "Cancel Contract - "
          ++ "Account or Contract Essentials failed"

-- Leaves a contract in the right time. Differently from cancelContract,
-- a user that uses leaveContract is not involved in any accusation, is
-- not supposed to deliver any service and is not supposed to judge any
-- conflict. He can, therefore, return with his trust tokens, his SIG and,
-- if he is a publisher, the price value
leaveContract :: AccountSettings -> Review -> AssetClass -> Contract w s Text ()
leaveContract accountSettings review contractNFT = do
  -- The signer public key hash
  pkh <- pubKeyHash <$> Contract.ownPubKey

  -- Tries to get the account off-chain essentials
  maybeAccountOffChainEssentials <-
    getAccountOffChainEssentials accountSettings pkh

  -- Tries to get the contract off-chain essentials
  maybeContractOffChainEssentials <-
    getContractOffChainEssentials accountSettings contractNFT

  -- Verifies if both account and contract important information were gathered.
  -- If they all were, completes the transaction. Otherwise, logs an error and
  -- leaves.
  case (maybeAccountOffChainEssentials, maybeContractOffChainEssentials) of
    (Just aoe, Just coe) -> do
      -- Submit transaction
      ledgerTx <- submitTxConstraintsWith @ContractType lookups tx

      -- Wait until transaction is confirmed
      awaitTxConfirmed $ txId ledgerTx

      logInfo @String $
        "Leave Contract - Successfully left contract " ++ show contractNFT
      where
        -- The currency symbol used in every SIG token
        sigSymbol :: CurrencySymbol
        sigSymbol = asSignatureSymbol accountSettings

        -- The information needed in all the platform
        platformSettings :: PlatformSettings
        platformSettings = asPlatformSettings accountSettings

        reviewPercentage :: R.Rational
        reviewPercentage = psReviewPercentageOfTrust platformSettings

        trustAmount :: Integer
        trustAmount = sTrust (cdService contractDatum)

        reviewAmount :: Integer
        reviewAmount =
          R.round $
            (rScore review R.% 50)
              * reviewPercentage
              * R.fromInteger trustAmount

        -- The script validator hashes from the platform accounts and contracts
        accValHash, contrValHash :: ValidatorHash
        accValHash = accountValidatorHash accountSettings
        contrValHash = coeContractValidatorHash coe

        -- A reference to the account and contract UTxO we are trying to consume
        accountReference, contractReference :: TxOutRef
        accountReference = aoeAccountReference aoe
        contractReference = coeContractReference coe

        -- A version of the UTxOs we are consuming easy to be digested by the
        -- blockchain
        accountChainIndexTxOut, contractChainIndexTxOut :: ChainIndexTxOut
        accountChainIndexTxOut = fst (aoeAccountOutTx aoe)
        contractChainIndexTxOut = fst (coeContractOutTx coe)

        -- A version of the UTxOs we are consuming easy to be read and
        -- manipulated
        accountOut, contractOut :: TxOut
        accountOut = toTxOut accountChainIndexTxOut
        contractOut = toTxOut contractChainIndexTxOut

        -- The data from the account we are consuming
        accountDatum :: AccountDatum
        accountDatum = aoeAccountDatum aoe

        -- The data from the contract we are consuming
        contractDatum :: ContractDatum
        contractDatum = coeContractDatum coe

        -- The information needed to transact with a contract
        contractSett :: ContractSettings
        contractSett = coeContractSettings coe

        -- The values, which will either be sent or consumed by the contract
        -- and account
        sigValue, trustValue, reviewValue, txFeesValue :: Value
        sigValue = singleton sigSymbol (makeSigToken pkh accValHash) 1
        trustValue =
          assetClassValue
            (psToken platformSettings)
            (sTrust $ cdService contractDatum)
        reviewValue = assetClassValue (psToken platformSettings) reviewAmount
        txFeesValue =
          assetClassValue
            (psToken platformSettings)
            (psTxFee platformSettings)

        -- The resulting value from contract and account
        contractValue, accountValue :: Value
        -- In this case our contract loses the user's SIG token and his
        -- collateral, since everything went well for him
        contractValue =
          txOutValue contractOut
            <> negate trustValue
            <> negate sigValue
        -- The account value, simmilarly to the other transactions, loses get's
        -- the SIG token back and earn transaction fees
        accountValue =
          txOutValue accountOut
            <> sigValue
            <> reviewValue
            <> txFeesValue

        -- The data that will be given to the newly created contract UTxO
        -- In this case, the user is being removed from the contract role map
        newContractDatum :: ContractDatum
        newContractDatum = removeUser pkh contractDatum

        -- The data that will be given to the newly created account UTxO
        -- In this case, the contract is being removed from the account register
        -- of contracts and the CAS is being increased because everything went
        -- well
        newAccountDatum :: AccountDatum
        newAccountDatum = addReviewAD
          where
            removeContractAD :: AccountDatum
            removeContractAD = removeContract accountDatum contrValHash

            leaveContractCASAD :: AccountDatum
            leaveContractCASAD =
              leaveContractCAS (psCASMap platformSettings) removeContractAD

            addReviewAD :: AccountDatum
            addReviewAD = addReview leaveContractCASAD review

        lookups :: ScriptLookups ContractType
        lookups =
          Constraints.unspentOutputs
            ( Map.fromList
                [ (accountReference, accountChainIndexTxOut),
                  (contractReference, contractChainIndexTxOut)
                ]
            )
            <> Constraints.otherScript (accountValidator accountSettings)
            <> Constraints.otherScript (contractValidator contractSett)
            <> Constraints.typedValidatorLookups
              (typedContractValidator contractSett)

        tx :: TxConstraints (RedeemerType ContractType) (DatumType ContractType)
        tx =
          Constraints.mustBeSignedBy pkh
            <> Constraints.mustSpendScriptOutput
              accountReference
              (Redeemer $ PlutusTx.toBuiltinData (AReturn ARTLeave))
            <> Constraints.mustSpendScriptOutput
              contractReference
              (Redeemer $ PlutusTx.toBuiltinData (CLeave review))
            <> Constraints.mustPayToTheScript newContractDatum contractValue
            <> Constraints.mustPayToOtherScript
              accValHash
              (Datum $ PlutusTx.toBuiltinData newAccountDatum)
              accountValue
    _ ->
      logError @String $
        "Leave Contract - "
          ++ "Account or Contract Essentials failed"

type ContractSchema =
  Endpoint "create-contract" (AccountSettings, ContractDatum)
    .\/ Endpoint "sign" (Role, AccountSettings, AssetClass)
    .\/ Endpoint "cancel" (AccountSettings, AssetClass)
    .\/ Endpoint "leave" (AccountSettings, Review, AssetClass)

contractEndpoints :: Contract (Last AssetClass) ContractSchema Text ()
contractEndpoints =
  forever $
    handleError logError $
      awaitPromise $
        create' `select` sign' `select` cancel' `select` leave'
  where
    create' = endpoint @"create-contract" $ \(as, dat) -> createContract as dat
    sign' = endpoint @"sign" $ \(r, as, ac) -> signContract r as ac
    cancel' = endpoint @"cancel" $ \(as, ac) -> cancelContract as ac
    leave' = endpoint @"leave" $ \(as, rev, ac) -> leaveContract as rev ac
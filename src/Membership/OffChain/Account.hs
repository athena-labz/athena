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

module Membership.OffChain.Account where

import Control.Monad (forever)
import qualified Data.Map as Map
import Data.Monoid as M (Last (Last), Monoid (mconcat))
import Data.Text (Text, pack)
import Ledger
  ( Datum (Datum),
    PubKeyHash,
    Redeemer (Redeemer),
    TxOut (txOutValue),
    TxOutRef,
    TxOutTx (..),
    lookupDatum,
    pubKeyHash,
    txId,
  )
import Ledger.Constraints as Constraints
  ( ScriptLookups,
    TxConstraints,
    mustBeSignedBy,
    mustPayToOtherScript,
    mustPayToTheScript,
    mustSpendScriptOutput,
    otherScript,
    typedValidatorLookups,
    unspentOutputs,
  )
import Ledger.Typed.Scripts (ValidatorTypes (..))
import Ledger.Value
  ( AssetClass (AssetClass),
    CurrencySymbol,
    Value,
    assetClassValue,
    assetClassValueOf,
    singleton,
  )
import Membership.Account
  ( AccountDatum (adReviewCredit),
    AccountRedeemer (ACollect, ACreate, ASign),
    AccountType,
    contractCreationCAS,
    findAccountDatum,
  )
import Membership.Contract
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
    utxoAt,
    type (.\/),
  )
import Plutus.Contracts.Currency as Currency
  ( CurrencyError,
    OneShotCurrency,
    currencySymbol,
    mintContract,
    mintedValue,
  )
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AM
import PlutusTx.Prelude
  ( AdditiveGroup ((-)),
    Bool,
    Eq ((==)),
    Integer,
    Maybe (..),
    length,
    map,
    negate,
    return,
    traceError,
    ($),
    (++),
    (.),
    (<$>),
  )
import Wallet.Emulator.Wallet ()
import Prelude (Semigroup (..), Show (..), String, uncurry)

-- Returns all accounts, their Datum, the Value of fees they hold and their owners
{-# INLINEABLE findAccounts #-}
findAccounts :: AccountSettings -> Contract w s Text [(TxOutRef, TxOutTx, AccountDatum, Value, PubKeyHash)]
findAccounts as = do
  utxos <- utxoAt (accountAddress as)
  return $ map f (Map.toList utxos)
  where
    ps :: PlatformSettings
    ps = asPlatformSettings as

    sigSym :: CurrencySymbol
    sigSym = asSignatureSymbol as

    dsetTokens :: TxOutTx -> Integer
    dsetTokens o = assetClassValueOf (txOutValue $ txOutTxOut o) (psToken ps)

    sigPubKey :: Value -> PubKeyHash
    sigPubKey v = case findSignatory sigSym v of
      Just pkh -> pkh
      _ -> traceError "invalid account"

    f :: (TxOutRef, TxOutTx) -> (TxOutRef, TxOutTx, AccountDatum, Value, PubKeyHash)
    f (oref, o) = case ( findSignatories sigSym (txOutValue $ txOutTxOut o),
                         findAccountDatum (txOutTxOut o) (lookupDatum (txOutTxTx o))
                       ) of
      ([], Just dat) ->
        ( oref,
          o,
          dat,
          txOutValue (txOutTxOut o)
            <> negate (assetClassValue (psToken ps) (dsetTokens o)),
          sigPubKey $ txOutValue (txOutTxOut o)
        )
      (_, Just dat) ->
        ( oref,
          o,
          dat,
          txOutValue (txOutTxOut o)
            <> negate (assetClassValue (psToken ps) (dsetTokens o - adReviewCredit dat)),
          sigPubKey $ txOutValue (txOutTxOut o)
        )
      _ -> traceError "invalid account"

-- Return the UTxO from the account with a PubKeyHash
{-# INLINEABLE findAccount #-}
findAccount :: PubKeyHash -> AccountSettings -> Contract w s Text (Maybe (TxOutRef, TxOutTx))
findAccount owner as = do
  utxos <- Map.filter f <$> utxoAt (accountAddress as)
  return $ case Map.toList utxos of
    [(oref, o)] -> Just (oref, o)
    _ -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = findSignatories (asSignatureSymbol as) (txOutValue $ txOutTxOut o) == [owner]

accountLookups ::
  AccountSettings ->
  TxOutRef ->
  TxOutTx ->
  ScriptLookups AccountType
accountLookups as oref oTx =
  Constraints.unspentOutputs (Map.singleton oref oTx)
    <> Constraints.typedValidatorLookups (typedAccountValidator as)
    <> Constraints.otherScript (accountValidator as)

spendAccount ::
  TxOutRef ->
  AccountRedeemer ->
  TxConstraints (RedeemerType AccountType) (DatumType AccountType)
spendAccount oref r = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData r)

spendContract ::
  TxOutRef ->
  ContractRedeemer ->
  TxConstraints (RedeemerType ContractType) (DatumType ContractType)
spendContract oref r = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData r)

createContract :: AccountSettings -> ContractDatum -> Contract (Last AssetClass) s Text ()
createContract accountSettings dat = do
  pkh <- pubKeyHash <$> Contract.ownPubKey

  maybeAccount <- findAccount pkh accountSettings

  case maybeAccount of
    Just (accountOutputReference, TxOutTx accountOutputTx accountOutput) -> do
      case findAccountDatum accountOutput (lookupDatum accountOutputTx) of
        Just accountDatum -> do
          contractNFT <-
            mapError
              (pack . show)
              (mintContract pkh [("", 1)] :: Contract w s CurrencyError OneShotCurrency)
          let sigSym = asSignatureSymbol accountSettings
              platformSettings = asPlatformSettings accountSettings
              contractNFTSymbol = Currency.currencySymbol contractNFT
              accValHash = accountValidatorHash accountSettings
              contractValue =
                singleton sigSym (makeSigToken pkh accValHash) 1
                  <> Currency.mintedValue contractNFT
              accountValue =
                txOutValue accountOutput
                  <> negate (singleton sigSym (makeSigToken pkh accValHash) 1)
                  <> assetClassValue (psToken platformSettings) (psTxFee platformSettings)
              lookups =
                accountLookups
                  accountSettings
                  accountOutputReference
                  (TxOutTx accountOutputTx accountOutput)
              tx =
                Constraints.mustBeSignedBy pkh
                  <> spendAccount accountOutputReference ACreate
                  <> Constraints.mustPayToTheScript (contractCreationCAS accountDatum) accountValue
                  <> Constraints.mustPayToOtherScript
                    (asContractValidatorHash accountSettings)
                    (Datum $ PlutusTx.toBuiltinData dat)
                    contractValue
          _ <- submitTxConstraintsWith @AccountType lookups tx
          tell $ Last $ Just (AssetClass (contractNFTSymbol, ""))
          logInfo @String $ show (AssetClass (contractNFTSymbol, ""))
          logInfo @String $ "created contract"
        Nothing -> logError @String "no datum found"
    Nothing -> logError @String "no account found"

signContract :: Role -> AccountSettings -> AssetClass -> Contract w s Text ()
signContract userRole accountSettings contractNFT = do
  -- The signer public key hash
  pkh <- pubKeyHash <$> Contract.ownPubKey

  -- Tries to get the account that belongs to this user
  maybeAccount <- findAccount pkh accountSettings

  -- Tries to get the contract corresponding to this NFT
  maybeContract <- findContract contractNFT (asContractValidatorHash accountSettings)

  case (maybeAccount, maybeContract) of
    (Just (accountReference, accountOutTx), Just (contractReference, contractOutTx)) -> do
      let accountTx = txOutTxTx accountOutTx
          accountOut = txOutTxOut accountOutTx
          contractTx = txOutTxTx contractOutTx
          contractOut = txOutTxOut contractOutTx

          maybeAccountDatum = findAccountDatum accountOut (lookupDatum accountTx)
          maybeContractDatum = findContractDatum contractOut (lookupDatum contractTx)

      case (maybeAccountDatum, maybeContractDatum) of
        (Just accountDatum, Just contractDatum) -> do
          let sigSym = asSignatureSymbol accountSettings
              platformSettings = asPlatformSettings accountSettings
              accValHash = accountValidatorHash accountSettings

              sigValue = singleton sigSym (makeSigToken pkh accValHash) 1
              trustValue = assetClassValue (psToken platformSettings) (sTrust $ cdService contractDatum)
              txFeesValue = assetClassValue (psToken platformSettings) (psTxFee platformSettings)

              contractValue = txOutValue contractOut <> sigValue <> trustValue
              accountValue = txOutValue accountOut <> negate sigValue <> txFeesValue

              contractSett = contractSettings (psToken platformSettings) sigSym
              newContractDatum = addUser pkh userRole contractDatum

              lookups =
                Constraints.unspentOutputs (Map.fromList
                  [ (accountReference, accountOutTx),
                    (contractReference, contractOutTx)
                  ])
                  <> Constraints.otherScript (accountValidator accountSettings)
                  <> Constraints.otherScript (contractValidator contractSett)
                  <> Constraints.typedValidatorLookups (typedContractValidator contractSett)
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
                    (Datum $ PlutusTx.toBuiltinData (contractCreationCAS accountDatum))
                    accountValue
          _ <- submitTxConstraintsWith @ContractType lookups tx
          logInfo @String $ "signed contract"
        _ -> logError @String "account or contract datum not found"
    (Nothing, _) -> logError @String "account not found"
    _ -> logError @String "contract not found"

accuse :: PubKeyHash -> Integer -> AccountSettings -> AssetClass -> Contract w s Text ()
accuse accused judgePrice accountSettings contractNFT = do
  -- The accuser public key hash
  pkh <- pubKeyHash <$> Contract.ownPubKey

  -- Current POSIXTime
  curTime <- Contract.currentTime

  -- The contract with the given NFT
  maybeContract <- findContract contractNFT (asContractValidatorHash accountSettings)

  case maybeContract of
    Just (contractReference, contractOutTx) -> do
      let contractTx = txOutTxTx contractOutTx
          contractOut = txOutTxOut contractOutTx

          maybeContractDatum = findContractDatum contractOut (lookupDatum contractTx)

      case maybeContractDatum of
        Just contractDatum -> do
          let sigSym = asSignatureSymbol accountSettings
              platformSettings = asPlatformSettings accountSettings
              accValHash = accountValidatorHash accountSettings

              judgeValue = assetClassValue (psToken platformSettings) judgePrice
              contractValue = txOutValue contractOut <> judgeValue

              contractSett = contractSettings (psToken platformSettings) sigSym
              newContractDatum = accuseUser pkh accused curTime contractDatum
              lookups =
                Constraints.unspentOutputs (Map.fromList
                  [ (contractReference, contractOutTx)
                  ])
                  <> Constraints.otherScript (contractValidator contractSett)
                  <> Constraints.typedValidatorLookups (typedContractValidator contractSett)
              tx =
                Constraints.mustBeSignedBy pkh
                  <> Constraints.mustSpendScriptOutput
                      contractReference
                      (Redeemer $ PlutusTx.toBuiltinData $ CAccuse pkh accused)
                  <> Constraints.mustPayToTheScript newContractDatum contractValue
          _ <- submitTxConstraintsWith @ContractType lookups tx
          logInfo @String $ "accused user"
        _ -> logError @String "contract datum not found"
    _ -> logError @String "contract not found"

collectFees :: AccountSettings -> Contract (Last AssetClass) s Text ()
collectFees as = do
  xs <- findAccounts as
  case xs of
    [] -> logInfo @String "no accounts found"
    _ -> do
      let lookups =
            Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _, _, _) <- xs])
              <> Constraints.otherScript (accountValidator as)
          tx =
            M.mconcat
              [ Constraints.mustSpendScriptOutput
                  oref
                  $ Redeemer $ PlutusTx.toBuiltinData (ACollect wPkh)
                | (oref, _, _, _, wPkh) <- xs
              ]
              <> M.mconcat
                [ Constraints.mustPayToOtherScript
                    (accountValidatorHash as)
                    (Datum $ PlutusTx.toBuiltinData dat)
                    v
                  | (_, _, dat, v, _) <- xs
                ]
      ledgerTx <- submitTxConstraintsWith @AccountType lookups tx
      awaitTxConfirmed $ txId ledgerTx
      logInfo @String $ "collected from " ++ show (length xs) ++ " account(s)"

type AccountSchema =
  Endpoint "create" (AccountSettings, ContractDatum)
    .\/ Endpoint "collect" AccountSettings

type ContractSchema =
  Endpoint "sign" (Role, AccountSettings, AssetClass)
    .\/ Endpoint "accuse" (PubKeyHash, Integer, AccountSettings, AssetClass)

accountEndpoint :: Contract (Last AssetClass) AccountSchema Text ()
accountEndpoint =
  forever $
    handleError logError $
      awaitPromise $
        create' `select` collect'
  where
    create' = endpoint @"create" $ uncurry createContract
    collect' = endpoint @"collect" $ \ps -> collectFees ps

contractEndpoint :: Contract () ContractSchema Text ()
contractEndpoint =
  forever $
    handleError logError $
      awaitPromise $
        sign' `select` accuse'
  where
    sign' = endpoint @"sign" $ \(r, ps, ac) -> signContract r ps ac
    accuse' = endpoint @"accuse" $ \(acd, judPrice, ps, ac) -> accuse acd judPrice ps ac

-- collectEndpoint :: Contract () AccountSchema Text ()
-- collectEndpoint = forever $
--   handleError logError $
--     awaitPromise $
--       endpoint @"collect" $ \ps -> collectFees ps
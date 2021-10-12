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

module Membership.OffChain.Logic where

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
    mintingPolicy,
    mustBeSignedBy,
    mustMintValue,
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
  ( AccountDatum (..),
    AccountOffChainEssentials (..),
    AccountRedeemer (..),
    AccountReturnType (..),
    AccountType,
    addContract,
    addReview,
    contractCreationCAS,
    declaredGuiltyCAS,
    findAccountDatum,
    removeContract,
    signContractCAS,
  )
import Membership.Contract
import Membership.Logic
import Membership.OffChain.Utils
import Membership.OnChain.Account
import Membership.OnChain.Contract
import Membership.OnChain.Logic
import Membership.OnChain.ShameToken
import Membership.PlatformSettings
import Membership.Service
import Membership.ShameToken
import Membership.Signature
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
    utxosTxOutTxAt,
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
    BuiltinByteString,
    Eq ((==)),
    Integer,
    Maybe (..),
    filter,
    fst,
    isJust,
    isNothing,
    length,
    map,
    mempty,
    negate,
    otherwise,
    return,
    snd,
    traceError,
    ($),
    (&&),
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
import Prelude (Semigroup (..), Show (..), String, uncurry)

-- There should be one unique logic for this user with the same hash, since
-- different logics and inputs would make the hash different

-- Based on the needed settings and based on a key (an arbitrary bytestring used
-- to differentiate logics), createLogic mints a new shame token (a token used to
-- identify the logic and that will be transfered to the guilty party) and creates
-- a new UTxO with the logic address, giving it the shame token
createLogic ::
  AccountSettings ->
  LogicSettings ->
  BuiltinByteString ->
  Contract (Last AssetClass) s Text ()
createLogic accountSettings logicSettings key = do
  -- The public key hash from the user trying to create a logic
  pkh <- pubKeyHash <$> Contract.ownPubKey

  let -- The information needed in all the platform
      platformSettings :: PlatformSettings
      platformSettings = asPlatformSettings accountSettings

      -- The currency symbol from the SIG and the shame token
      sigSymbol, stSymbol :: CurrencySymbol
      sigSymbol = asSignatureSymbol accountSettings
      stSymbol = shameTokenCurrencySymbol

      -- The script validator hash from the logic that we are creating
      -- It varies depending on the logic settings and, therefore, varies
      -- depending on the logic conditions and results
      logValHash :: ValidatorHash
      logValHash = logicValHash logicSettings

      -- The data that the UTxO we are creating will habe, in this case, the state
      -- in which the logic should start
      logicDatum :: LogicState
      logicDatum = LSWaitingStart

      -- Because the shame token contains in it's token name some information
      -- regarding the creation of this token, ShameToken is a data type that
      -- digests all this information. In our case, st contains the user public
      -- key hash, the validator hash from the logic script to which the token
      -- will be sent after it's minted and the key chosen by the user
      st :: ShameToken
      st = shameToken pkh logValHash key

      -- The asset class from our shame token
      shameTokenAssetClass :: AssetClass
      shameTokenAssetClass = shameAssetClass stSymbol pkh logValHash key

      -- The value corresponding to a single shame token
      shameTokenValue :: Value
      shameTokenValue = shameValue stSymbol st

      -- The value we expect our logic script to have when we create it, in this
      -- case just a single shame token used to identify it
      logicValue :: Value
      logicValue = shameTokenValue

      lookups :: ScriptLookups AccountType
      lookups =
        Constraints.mintingPolicy shameTokenPolicy

      tx :: TxConstraints (RedeemerType AccountType) (DatumType AccountType)
      tx =
        Constraints.mustBeSignedBy pkh
          <> Constraints.mustMintValue shameTokenValue
          <> Constraints.mustPayToOtherScript
            logValHash
            (Datum $ PlutusTx.toBuiltinData logicDatum)
            logicValue

  -- Submit transaction
  ledgerTx <- submitTxConstraintsWith @AccountType lookups tx

  -- -- Wait until transaction is confirmed
  awaitTxConfirmed $ txId ledgerTx

  -- Make the shame token asset class accessible from the emulator trace
  tell $ Last $ Just shameTokenAssetClass

  logInfo @String $
    "Create Logic - Logic succefully created"

-- Given the necessary settings, the accused and the asset class which
-- identifies the contract that was broken by the accused, creates a new
-- accusation, by transfering the accused SIG and trust token to the logic
-- script, as well as the judge's SIG and trust token. Lastly, it also changes
-- the contract datum, adding an accusation to the list
accuse ::
  PubKeyHash ->
  AccountSettings ->
  LogicSettings ->
  AssetClass ->
  AssetClass ->
  Contract w s Text ()
accuse
  accused
  accountSettings
  logicSettings
  contractNFT
  shameTokenAssetClass = do
    -- The accuser public key hash
    accuser <- pubKeyHash <$> Contract.ownPubKey

    -- Current POSIXTime
    curTime <- Contract.currentTime

    -- Tries to get the contract off-chain essentials
    maybeContractOffChainEssentials <-
      getContractOffChainEssentials accountSettings contractNFT

    -- Tries to get the logic off-chain essentials
    maybeLogicOffChainEssentials <-
      getLogicOffChainEssentials logicSettings shameTokenAssetClass

    case (maybeContractOffChainEssentials, maybeLogicOffChainEssentials) of
      (Just coe, Just loe) -> do
        case tx of
          Just tx' -> do
            -- Submit transaction
            ledgerTx <- submitTxConstraintsWith @LogicType lookups tx'

            -- Wait until transaction is confirmed
            awaitTxConfirmed $ txId ledgerTx

            logInfo @String $ "Accuse - Successfuly accused user " ++ show accused
          Nothing
            | isNothing maybeAccuserRole -> logError @String "Accuse - Accuser not registered"
            | isNothing maybeAccusedRole -> logError @String "Accuse - Accused not registered"
            | isNothing maybeJudgePKH -> logError @String "Accuse - No valid judge found"
            | otherwise -> logError @String "Accuse - Accusation failed for unknown reasons"
        where
          -- ! Do not trace errors inside off-chain code.
          -- ! Do not trace errors inside off-chain code.
          -- ! Do not trace errors inside off-chain code.

          -- The currency symbol used in every SIG token
          sigSymbol :: CurrencySymbol
          sigSymbol = asSignatureSymbol accountSettings

          -- The information needed in all the platform
          platformSettings :: PlatformSettings
          platformSettings = asPlatformSettings accountSettings

          -- The token used to transact in DigiServices (DSET)
          platformToken :: AssetClass
          platformToken = psToken platformSettings

          -- The arbitrary bytestring used to differentiate logics embeded in
          -- the shame token name
          key :: BuiltinByteString
          key = stKey $ makeShameToken (snd $ unAssetClass shameTokenAssetClass)

          -- The script validator hash from the scripts we are trying to consume,
          -- which vary depending on the given settings
          accValHash, contrValHash, logValHash :: ValidatorHash
          accValHash = accountValidatorHash accountSettings
          contrValHash = asContractValidatorHash accountSettings
          logValHash = loeLogicValidatorHash loe

          -- A reference to the logic and contract UTxO we are trying to consume
          logicReference, contractReference :: TxOutRef
          logicReference = loeLogicReference loe
          contractReference = coeContractReference coe

          -- A version of the UTxOs we are consuming easy to be digested by the
          -- blockchain
          logicChainIndexTxOut, contractChainIndexTxOut :: ChainIndexTxOut
          logicChainIndexTxOut = fst (loeLogicOutTx loe)
          contractChainIndexTxOut = fst (coeContractOutTx coe)

          -- A version of the UTxOs we are consuming easy to read and
          -- manipulate
          logicOut, contractOut :: TxOut
          logicOut = toTxOut logicChainIndexTxOut
          contractOut = toTxOut contractChainIndexTxOut

          -- The data contained inside the contract we are consuming
          contractDatum :: ContractDatum
          contractDatum = coeContractDatum coe

          -- If our accuser is registered in the contract data, his role
          maybeAccuserRole :: Maybe Role
          maybeAccuserRole = AM.lookup accuser (cdRoleMap contractDatum)

          -- If our accused is registered in the contract data, his role
          maybeAccusedRole :: Maybe Role
          maybeAccusedRole = AM.lookup accused (cdRoleMap contractDatum)

          -- If we have both accused and accuser roles, construct an accusation,
          -- which is a data type that holds their public key hashes, roles and
          -- when the accusation was sent
          maybeAccusation :: Maybe Accusation
          maybeAccusation = do
            accuserRole <- maybeAccuserRole
            accusedRole <- maybeAccusedRole
            return $
              Accusation
                { aAccuser = (accuser, accuserRole),
                  aAccused = (accused, accusedRole),
                  aTime = curTime
                }

          -- If we have both accuser and accused role, construct a new datum,
          -- adding a new accusation to the list
          maybeNewContractDatum :: Maybe ContractDatum
          maybeNewContractDatum = do
            accusation <- maybeAccusation
            return $ accuseUser accusation contractDatum

          -- The information needed to transact with a contract
          contractSett :: ContractSettings
          contractSett = coeContractSettings coe

          -- All the sig tokens that can be found in the contract output we are
          -- trying to consume in their "digested form"
          sigs :: [Sig]
          sigs =
            findSignatures
              (lsSignatureSymbol logicSettings)
              (txOutValue contractOut)

          -- The judges information from the data of the contract we are
          -- consuming
          judges :: Judges
          judges = cdJudges contractDatum

          -- If there is a valid judge (one that signed our contract), get's the
          -- one that is supposed to mediate this conflict
          maybeJudgePKH :: Maybe PubKeyHash
          maybeJudgePKH = firstValidJudge (jsPubKeyHashes judges) sigs

          -- If there is a valid judge, this variable will hold the essential
          -- information about him, such as his price and in how much time he
          -- must make a verdict
          maybeJudge :: Maybe Judge
          maybeJudge = do
            judgePKH <- maybeJudgePKH
            return $
              Judge
                { jPubKeyHash = judgePKH,
                  jPrice = jsPrice judges,
                  jMaxDuration = jsMaxDuration judges
                }

          -- The state in which our resulting logic ouput should be. This can
          -- only be if we have a valid judge
          maybeLogicDatum :: Maybe LogicState
          maybeLogicDatum = do
            judge <- maybeJudge
            accusation <- maybeAccusation
            return $ LSWaitingVerdict contractNFT judge accusation

          -- The price our judge will receive in case he does make a verdict
          judgePrice :: Integer
          judgePrice = jsPrice $ cdJudges contractDatum

          -- The SIG token value of our judge, if there is one
          maybeJudgeSigValue :: Maybe Value
          maybeJudgeSigValue = do
            judgePKH <- maybeJudgePKH
            return $ singleton sigSymbol (makeSigToken judgePKH accValHash) 1

          -- The SIG token value of our accused
          accusedSigValue :: Value
          accusedSigValue = singleton sigSymbol (makeSigToken accused accValHash) 1

          -- Makes the trust and judge reward into a value
          judgeValue, trustValue :: Value
          judgeValue = assetClassValue platformToken judgePrice
          trustValue = assetClassValue platformToken (sTrust $ cdService contractDatum)

          -- If we do have a judge sig, the new logic and contract values
          -- In this case, our contract will lose the sig tokens from the judge
          -- and the accused, as well as, their trust tokens
          -- Our logic, in the other hand, will receive those four values from
          -- the contract plus the (expected) judge reward from the accuser own
          -- wallet
          maybeContractValue, maybeLogicValue :: Maybe Value
          maybeContractValue = do
            judgeSigValue <- maybeJudgeSigValue
            return $
              txOutValue contractOut
                <> negate
                  ( trustValue
                      <> trustValue
                      <> judgeSigValue
                      <> accusedSigValue
                  )
          maybeLogicValue = do
            judgeSigValue <- maybeJudgeSigValue
            return $
              txOutValue logicOut
                <> trustValue
                <> trustValue
                <> judgeValue
                <> judgeSigValue
                <> accusedSigValue

          lookups :: ScriptLookups LogicType
          lookups =
            Constraints.unspentOutputs
              ( Map.fromList
                  [ (contractReference, contractChainIndexTxOut),
                    (logicReference, logicChainIndexTxOut)
                  ]
              )
              <> Constraints.otherScript (contractValidator contractSett)
              <> Constraints.otherScript (logicValidator logicSettings)
              <> Constraints.typedValidatorLookups (typedLogicValidator logicSettings)

          tx :: Maybe (TxConstraints (RedeemerType LogicType) (DatumType LogicType))
          tx = do
            contractValue <- maybeContractValue
            logicValue <- maybeLogicValue
            accusation <- maybeAccusation
            logicDatum <- maybeLogicDatum
            newContractDatum <- maybeNewContractDatum

            return $
              Constraints.mustBeSignedBy accuser
                <> Constraints.mustSpendScriptOutput
                  contractReference
                  (Redeemer $ PlutusTx.toBuiltinData $ CAccuse accusation)
                <> Constraints.mustSpendScriptOutput
                  logicReference
                  (Redeemer $ PlutusTx.toBuiltinData $ LRAccuse accusation)
                <> Constraints.mustPayToOtherScript
                  contrValHash
                  (Datum $ PlutusTx.toBuiltinData newContractDatum)
                  contractValue
                <> Constraints.mustPayToTheScript logicDatum logicValue
      _ -> logError @String "Accuse - Contract or Logic essentials not found"

-- Changes the logic datum, giving the judges verdict. This datum will then be
-- used to ensure the correct distribution of tokens in the "collect phase"
mediate ::
  Verdict ->
  AccountSettings ->
  LogicSettings ->
  AssetClass ->
  Contract w s Text ()
mediate verdict accountSettings logicSettings shameTokenAssetClass = do
  -- The user that called this endpoint should be the judge
  judge <- pubKeyHash <$> Contract.ownPubKey

  -- Tries to get the logic off-chain essentials
  maybeLogicOffChainEssentials <-
    getLogicOffChainEssentials logicSettings shameTokenAssetClass

  -- If we could get the logic important information, use it to build the
  -- transaction, otherwise, log an error and leave
  case maybeLogicOffChainEssentials of
    Just loe -> do
      let -- The state our consumed logic is in (the logic script data)
          logicDatum :: LogicState
          logicDatum = loeLogicDatum loe

      -- If our logic data is, as expected, in the waiting verdict state,
      -- proceed with the transaction, otherwise log an error and leave
      case logicDatum of
        LSWaitingVerdict contractNFT judgeInfo accusation -> do
          -- Submit transaction
          ledgerTx <- submitTxConstraintsWith @LogicType lookups tx

          -- Wait until transaction is confirmed
          awaitTxConfirmed $ txId ledgerTx

          logInfo @String $ "Mediate - Conflict successfully mediated"
          where
            -- The currency symbol used in every SIG token
            sigSymbol :: CurrencySymbol
            sigSymbol = asSignatureSymbol accountSettings

            -- The information needed in all the platform
            platformSettings :: PlatformSettings
            platformSettings = asPlatformSettings accountSettings

            -- The token used to transact in DigiServices (DSET)
            platformToken :: AssetClass
            platformToken = psToken platformSettings

            -- The script validator hash from the scripts we are trying to
            -- consume, which vary depending on the given settings
            accValHash, contrValHash, logValHash :: ValidatorHash
            accValHash = accountValidatorHash accountSettings
            contrValHash = asContractValidatorHash accountSettings
            logValHash = loeLogicValidatorHash loe

            -- The output blockchain reference from the logic script we are
            -- consuming
            logicReference :: TxOutRef
            logicReference = loeLogicReference loe

            -- The blockchain digestable form of our logic input UTxO
            logicChainIndexTxOut :: ChainIndexTxOut
            logicChainIndexTxOut = fst (loeLogicOutTx loe)

            -- The easy-to-handle form of our logic input UTxO
            logicOut :: TxOut
            logicOut = toTxOut logicChainIndexTxOut

            -- The state / data our newly created logic should have
            newLogicDatum :: LogicState
            newLogicDatum =
              LSWaitingEnd contractNFT judgeInfo accusation verdict

            -- The value our newly created logic should have
            logicValue :: Value
            logicValue = txOutValue logicOut

            lookups :: ScriptLookups LogicType
            lookups =
              Constraints.unspentOutputs
                ( Map.fromList [(logicReference, logicChainIndexTxOut)]
                )
                <> Constraints.otherScript (logicValidator logicSettings)
                <> Constraints.typedValidatorLookups
                  (typedLogicValidator logicSettings)

            tx :: TxConstraints (RedeemerType LogicType) (DatumType LogicType)
            tx =
              Constraints.mustBeSignedBy judge
                <> Constraints.mustSpendScriptOutput
                  logicReference
                  (Redeemer $ PlutusTx.toBuiltinData $ LRMediate verdict)
                <> Constraints.mustPayToTheScript newLogicDatum logicValue
        _ -> logError @String "Mediate - Logic in wrong state"
    Nothing -> logError @String "Mediate - Logic essentials not found"

-- After the contract has been mediated, distributes the trust token according
-- to the distribution given by the verdict and logic
collect ::
  AccountSettings ->
  LogicSettings ->
  AssetClass ->
  AssetClass ->
  Contract w s Text ()
collect accountSettings logicSettings contractNFT shameTokenAssetClass = do
  -- The signer public key hash
  pkh <- pubKeyHash <$> Contract.ownPubKey

  -- Tries to get the contract off-chain essentials
  maybeContractOffChainEssentials <-
    getContractOffChainEssentials accountSettings contractNFT

  -- Tries to get the logic off-chain essentials
  maybeLogicOffChainEssentials <-
    getLogicOffChainEssentials logicSettings shameTokenAssetClass

  case (maybeContractOffChainEssentials, maybeLogicOffChainEssentials) of
    (Just coe, Just loe) -> do
      let -- The state our consumed logic is in (the logic script data)
          logicDatum :: LogicState
          logicDatum = loeLogicDatum loe

      -- If our logic data is, as expected, in the waiting verdict state,
      -- proceed with the transaction, otherwise log an error and leave
      case logicDatum of
        LSWaitingEnd contractNFT judge accusation verdict -> do
          -- If our user is guilty, a shame token should be transfered from the
          -- logic to his account, otherwise, it's only necessary to distribute
          -- the accused trust token accordinly and give the SIG tokens back to
          -- the involved parties (accused and mediator / judge)
          if guilty
            then do
              -- Tries to get the account off-chain essentials
              maybeAccountOffChainEssentials <-
                getAccountOffChainEssentials accountSettings accused

              case maybeAccountOffChainEssentials of
                Just aoe -> do
                  -- Submit transaction
                  ledgerTx <-
                    submitTxConstraintsWith @LogicType guiltyLookups guiltyTx

                  -- Wait until transaction is confirmed
                  awaitTxConfirmed $ txId ledgerTx

                  logInfo @String $
                    "Logic Collect - "
                      ++ "Collaterals and rewards successfully distributed"
                  where
                    accountChainIndexTxOut :: ChainIndexTxOut
                    accountChainIndexTxOut = fst (aoeAccountOutTx aoe)

                    accountOut :: TxOut
                    accountOut = toTxOut accountChainIndexTxOut

                    accountDatum :: AccountDatum
                    accountDatum = aoeAccountDatum aoe

                    accountReference :: TxOutRef
                    accountReference = aoeAccountReference aoe

                    accountValue :: Value
                    accountValue =
                      txOutValue accountOut
                        <> shameTokenValue
                        <> accusedSigValue

                    guiltyLookups :: ScriptLookups LogicType
                    guiltyLookups =
                      Constraints.unspentOutputs
                        ( Map.fromList
                            [ (accountReference, accountChainIndexTxOut),
                              (contractReference, contractChainIndexTxOut),
                              (logicReference, logicChainIndexTxOut)
                            ]
                        )
                        <> Constraints.otherScript
                          (accountValidator accountSettings)
                        <> Constraints.otherScript
                          (contractValidator contractSett)
                        <> Constraints.otherScript
                          (logicValidator logicSettings)
                        <> Constraints.typedValidatorLookups
                          (typedLogicValidator logicSettings)

                    guiltyTx ::
                      TxConstraints
                        (RedeemerType LogicType)
                        (DatumType LogicType)
                    guiltyTx =
                      distributionConstraints
                        <> Constraints.mustSpendScriptOutput
                          accountReference
                          (Redeemer $
                            PlutusTx.toBuiltinData
                              (AReturn ARTExpelled (fst $ aAccused accusation)))
                        <> Constraints.mustSpendScriptOutput
                          contractReference
                          (Redeemer $ PlutusTx.toBuiltinData CMediate)
                        <> Constraints.mustSpendScriptOutput
                          logicReference
                          (Redeemer $ PlutusTx.toBuiltinData LRConsume)
                        <> Constraints.mustPayToOtherScript
                          contrValHash
                          (Datum $ PlutusTx.toBuiltinData newContractDatum)
                          contractValue
                        <> Constraints.mustPayToOtherScript
                          accValHash
                          ( Datum $
                              PlutusTx.toBuiltinData
                                ( declaredGuiltyCAS
                                    (psCASMap platformSettings)
                                    (removeContract accountDatum contrValHash)
                                )
                          )
                          accountValue
                _ ->
                  logError @String $
                    "Logic Collect - "
                      ++ "Account Essentials not found"
            else do
              -- Submit transaction
              ledgerTx <-
                submitTxConstraintsWith @LogicType innocentLookups innocentTx

              -- Wait until transaction is confirmed
              awaitTxConfirmed $ txId ledgerTx

              logInfo @String $
                "Logic Collect - "
                  ++ "Collaterals and rewards successfully distributed"
          where
            sigSymbol :: CurrencySymbol
            sigSymbol = asSignatureSymbol accountSettings

            platformSettings :: PlatformSettings
            platformSettings = asPlatformSettings accountSettings

            platformToken :: AssetClass
            platformToken = psToken platformSettings

            accValHash :: ValidatorHash
            accValHash = accountValidatorHash accountSettings

            contrValHash :: ValidatorHash
            contrValHash = coeContractValidatorHash coe

            logicChainIndexTxOut, contractChainIndexTxOut :: ChainIndexTxOut
            logicChainIndexTxOut = fst (loeLogicOutTx loe)
            contractChainIndexTxOut = fst (coeContractOutTx coe)

            logicOut, contractOut :: TxOut
            logicOut = toTxOut logicChainIndexTxOut
            contractOut = toTxOut contractChainIndexTxOut

            contractDatum :: ContractDatum
            contractDatum = coeContractDatum coe

            contractReference, logicReference :: TxOutRef
            contractReference = coeContractReference coe
            logicReference = loeLogicReference loe

            contractSett :: ContractSettings
            contractSett = coeContractSettings coe

            trust :: Integer
            trust = sTrust $ cdService contractDatum

            accuser, accused :: PubKeyHash
            accuser = fst $ aAccuser accusation
            accused = fst $ aAccused accusation

            -- The proportion (Accused : Accuser) that should be distributed
            -- from the accused trust tokens according to the logic and verdict.
            -- (0 : 1), for example, means that the accused should get his
            -- tokens back because he is innocent
            proportion :: Maybe Proportion
            proportion =
              failedProportion
                (snd $ aAccused accusation)
                verdict
                (lsLogic logicSettings)

            -- If the proportion is not (0 : 1), we say the accused failed (he
            -- did something wrong) and should be out of the contract, losing
            -- tokens according to the proportion, but he is only guilty if the
            -- rule he broke has guiltEnabled (forcing him to receive the shame
            -- token)
            failed :: Bool
            failed = isJust proportion

            -- The value the judge should receive because of his services
            judgeValue :: Value
            judgeValue = assetClassValue platformToken (jPrice judge)

            accusedSigValue, judgeSigValue, trustValue, shameTokenValue :: Value
            accusedSigValue =
              singleton sigSymbol (makeSigToken accused accValHash) 1
            judgeSigValue =
              singleton
                sigSymbol
                (makeSigToken (jPubKeyHash judge) accValHash)
                1
            trustValue = assetClassValue (psToken platformSettings) trust
            shameTokenValue = assetClassValue shameTokenAssetClass 1

            -- If our service is one-time (therefore has a price), the accuser
            -- is a client and the accused is a publisher, the client should
            -- receive his money back, that's what priceValue accounts for
            priceValue :: Value
            priceValue = case sType (cdService contractDatum) of
              OneTime v _
                | (snd (aAccuser accusation) == Client)
                    && (snd (aAccused accusation) == Publisher) ->
                  v
              _ -> mempty

            -- Not only should the accused lose tokens and leave the contract,
            -- but should he also receive the shame token? (which will be stuck
            -- in his account for ever)
            guilty :: Bool
            guilty =
              isGuilty
                (snd $ aAccused accusation)
                (lsLogic logicSettings)
                verdict

            -- What we expected our consumed contract to lose or earn
            expectedContractValueDifference :: Value
            expectedContractValueDifference =
              if failed
                then
                  judgeSigValue
                    <> trustValue
                    <> negate priceValue
                else
                  accusedSigValue
                    <> judgeSigValue
                    <> trustValue
                    <> trustValue

            -- The new contract value
            contractValue :: Value
            contractValue =
              txOutValue contractOut
                <> expectedContractValueDifference

            -- The datum our new contract will have, with an accusation removed
            -- and the accused removed from the role map if he "failed" (broke
            -- at least one rule)
            newContractDatum :: ContractDatum
            newContractDatum =
              ContractDatum
                { cdJudges = cdJudges contractDatum,
                  cdLogicScript = cdLogicScript contractDatum,
                  cdAccusations =
                    cdAccusations (removeAccusation accusation contractDatum),
                  cdService = cdService contractDatum,
                  cdRoleMap =
                    if failed
                      then AM.delete accused (cdRoleMap contractDatum)
                      else cdRoleMap contractDatum
                }

            distributionConstraints ::
              TxConstraints (RedeemerType LogicType) (DatumType LogicType)
            distributionConstraints =
              case proportion of
                Just p ->
                  Constraints.mustPayToPubKey (jPubKeyHash judge) judgeValue
                    <> Constraints.mustPayToPubKey accuser accuserValue
                    <> Constraints.mustPayToPubKey accused accusedValue
                  where
                    accuserValue :: Value
                    accuserValue =
                      assetClassValue
                        platformToken
                        (fst $ trustProportion p trust)
                        <> priceValue

                    accusedValue :: Value
                    accusedValue =
                      assetClassValue
                        platformToken
                        (snd $ trustProportion p trust)
                Nothing ->
                  Constraints.mustPayToPubKey (jPubKeyHash judge) judgeValue

            innocentLookups :: ScriptLookups LogicType
            innocentLookups =
              Constraints.unspentOutputs
                ( Map.fromList
                    [ (contractReference, contractChainIndexTxOut),
                      (logicReference, logicChainIndexTxOut)
                    ]
                )
                <> Constraints.otherScript (contractValidator contractSett)
                <> Constraints.otherScript (logicValidator logicSettings)
                <> Constraints.typedValidatorLookups
                  (typedLogicValidator logicSettings)

            innocentTx ::
              TxConstraints
                (RedeemerType LogicType)
                (DatumType LogicType)
            innocentTx =
              distributionConstraints
                <> Constraints.mustSpendScriptOutput
                  contractReference
                  (Redeemer $ PlutusTx.toBuiltinData CMediate)
                <> Constraints.mustSpendScriptOutput
                  logicReference
                  (Redeemer $ PlutusTx.toBuiltinData LRConsume)
                <> Constraints.mustPayToOtherScript
                  contrValHash
                  (Datum $ PlutusTx.toBuiltinData newContractDatum)
                  contractValue
        _ -> logError @String "Logic Collect - Logic in wrong state"
    _ ->
      logError @String $
        "Logic Collect - "
          ++ "Contract or Logic essentials not found"

type LogicSchema =
  Endpoint "create-logic" (AccountSettings, LogicSettings, BuiltinByteString)
    .\/ Endpoint
          "accuse"
          (PubKeyHash, AccountSettings, LogicSettings, AssetClass, AssetClass)
    .\/ Endpoint "mediate" (Verdict, AccountSettings, LogicSettings, AssetClass)
    .\/ Endpoint "collect" (AccountSettings, LogicSettings, AssetClass, AssetClass)

logicEndpoints :: Contract (Last AssetClass) LogicSchema Text ()
logicEndpoints =
  forever $
    handleError logError $
      awaitPromise $
        createLogic' `select` accuse' `select` mediate' `select` collect'
  where
    createLogic' = endpoint @"create-logic" $ \(as, ls, key) -> createLogic as ls key
    accuse' = endpoint @"accuse" $ \(acd, as, ls, cAC, lAC) -> accuse acd as ls cAC lAC
    mediate' = endpoint @"mediate" $ \(ver, as, ls, lAC) -> mediate ver as ls lAC
    collect' = endpoint @"collect" $ \(as, ls, cAC, lAC) -> collect as ls cAC lAC
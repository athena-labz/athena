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

module Membership.OnChain.Logic where

import Ledger
  ( POSIXTime,
    PubKeyHash,
    ScriptContext (scriptContextTxInfo),
    TxInInfo (txInInfoResolved),
    TxInfo (txInfoValidRange),
    TxOut (txOutValue),
    Validator,
    ValidatorHash,
    Value,
    contains,
    findDatum,
    findOwnInput,
    from,
    getContinuingOutputs,
    to,
    txSignedBy,
    validatorHash,
    valuePaidTo,
  )
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value (assetClassValue, assetClassValueOf, geq)
import Membership.Account
  ( AccountDatum,
    declaredGuiltyCAS,
    findAccountDatum,
  )
import Membership.Contract
  ( Accusation (aAccused, aAccuser, aTime),
    ContractDatum (cdJudges, cdService),
    Judge (..),
    Judges (jsMaxDuration, jsPrice, jsPubKeyHashes),
    findContractDatum,
  )
import Membership.Logic
  ( LogicRedeemer (LRAccuse, LRConsume, LRMediate),
    LogicSettings
      ( lsContract,
        lsInputs,
        lsLogic,
        lsPlatformSettings,
        lsSignatureSymbol
      ),
    LogicState (..),
    Proportion,
    failedProportion,
    findLogicDatum,
    firstValidJudge,
    isGuilty,
    trustProportion,
    validLogicConditions,
  )
import Membership.OnChain.Utils (strictFindOutAndIn)
import Membership.PlatformSettings
  ( PlatformSettings (psShameToken, psToken),
  )
import Membership.Service (Service (sTrust))
import Membership.Signature
  ( Sig (sScript, sUser),
    findSignatures,
    signatureValue,
    signatureValue',
    whoSigned',
  )
import Membership.Utils
  ( strictFindInputWithValHash,
    strictFindOutputWithValHash,
  )
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Prelude
  ( AdditiveSemigroup ((+)),
    Bool (..),
    Eq ((==)),
    Integer,
    Maybe (..),
    all,
    find,
    fst,
    maybe,
    negate,
    not,
    null,
    snd,
    traceError,
    traceIfFalse,
    ($),
    (&&),
    (<>),
    (||),
  )

{-# INLINEABLE mkLogicValidator #-}
mkLogicValidator :: LogicSettings -> LogicState -> LogicRedeemer -> ScriptContext -> Bool
mkLogicValidator logicSettings LSWaitingStart (LRAccuse accusation) ctx =
  traceIfFalse "Logic Accusation - Not signed by accuser" signedByAccuser
    && traceIfFalse
      "Logic Accusation - Invalid logic conditions"
      (validLogicConditions (lsLogic logicSettings) (lsInputs logicSettings))
    && traceIfFalse "Logic Accusation - Invalid logic" validLogic
    && traceIfFalse "Logic Accusation - Invalid logic datum" validLogicDatum
    && traceIfFalse "Logic Accusation - Invalid logic value" validLogicValue
    && traceIfFalse "Logic Accusation - Invalid contract" validContract
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    logicDatum :: LogicState
    logicDatum = case findLogicDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Logic Accusation - Logic Datum could not be found"

    shameToken :: Value
    shameToken = assetClassValue (psShameToken $ lsPlatformSettings logicSettings) 1

    contractInput, contractOutput :: TxOut
    contractInput = case strictFindInputWithValHash (fst $ lsContract logicSettings) info of
      Just o -> o
      Nothing -> traceError "Logic Accusation - Couldn't find an unique contract input"
    contractOutput = case strictFindOutputWithValHash (fst $ lsContract logicSettings) info of
      Just o -> o
      Nothing -> traceError "Logic Accusation - Couldn't find an unique contract output"

    contractDatum :: ContractDatum
    contractDatum = case findContractDatum contractInput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Logic Accusation - Contract Datum could not be found"

    contractNFT :: Value
    contractNFT = assetClassValue (snd $ lsContract logicSettings) 1

    judges :: Judges
    judges = cdJudges contractDatum

    trustValue :: Value
    trustValue =
      assetClassValue
        (psToken $ lsPlatformSettings logicSettings)
        (sTrust $ cdService contractDatum)

    judgeValue :: Value
    judgeValue =
      assetClassValue
        (psToken $ lsPlatformSettings logicSettings)
        (jsPrice judges)

    sigs :: [Sig]
    sigs = findSignatures (lsSignatureSymbol logicSettings) (txOutValue ownInput)

    contractSigs :: [Sig]
    contractSigs = findSignatures (lsSignatureSymbol logicSettings) (txOutValue contractInput)

    ownSig :: Sig
    ownSig = case whoSigned' info sigs of
      Just s -> s
      Nothing -> traceError "Logic Accusation - SIG token missing"

    accountValHash :: ValidatorHash
    accountValHash = sScript ownSig

    judgePKH :: PubKeyHash
    judgePKH = case firstValidJudge (jsPubKeyHashes judges) contractSigs of
      Just pkh -> pkh
      Nothing -> traceError "Logic Accusation - No available judge"

    judge :: Judge
    judge =
      Judge
        { jPubKeyHash = judgePKH,
          jPrice = jsPrice judges,
          jMaxDuration = jsMaxDuration judges
        }

    judgeSigValue :: Value
    judgeSigValue =
      signatureValue
        (lsSignatureSymbol logicSettings)
        judgePKH
        accountValHash

    accusedSigValue :: Value
    accusedSigValue =
      signatureValue
        (lsSignatureSymbol logicSettings)
        (aAccused accusation)
        accountValHash

    signedByAccuser :: Bool
    signedByAccuser = aAccuser accusation == sUser ownSig

    validLogic :: Bool
    validLogic =
      txOutValue ownInput `geq` shameToken
        && txOutValue ownOutput `geq` shameToken

    validLogicDatum :: Bool
    validLogicDatum =
      logicDatum == LSWaitingVerdict judge accusation

    validLogicValue :: Bool
    validLogicValue =
      txOutValue ownOutput <> negate (txOutValue ownInput)
        == trustValue -- The accused collateral
        <> trustValue -- The judge collateral
        <> judgeValue -- The judge price
        <> judgeSigValue
        <> accusedSigValue

    validContract :: Bool
    validContract =
      txOutValue contractInput `geq` contractNFT
        && txOutValue contractOutput `geq` contractNFT
mkLogicValidator
  logicSettings
  (LSWaitingVerdict judge accusation)
  (LRMediate verdict)
  ctx =
    traceIfFalse "Logic Mediate - Invalid logic" validLogic
      && traceIfFalse "Logic Mediate - Invalid logic datum" validLogicDatum
      && traceIfFalse "Logic Mediate - Invalid logic value" validLogicValue
      && traceIfFalse "Logic Mediate - Deadline passed" (not $ from deadline `contains` txInfoValidRange info)
      && traceIfFalse "Logic Mediate - Not signed by judge" signedByJudge
      && traceIfFalse "Logic Mediate - Invalid verdict" validVerdict
    where
      info :: TxInfo
      info = scriptContextTxInfo ctx

      ownInput, ownOutput :: TxOut
      ownInput = fst $ strictFindOutAndIn ctx
      ownOutput = snd $ strictFindOutAndIn ctx

      logicDatum :: LogicState
      logicDatum = case findLogicDatum ownOutput (`findDatum` info) of
        Just dat -> dat
        Nothing -> traceError "Logic Mediate - Logic Datum could not be found"

      shameToken :: Value
      shameToken = assetClassValue (psShameToken $ lsPlatformSettings logicSettings) 1

      judgeValue :: Value
      judgeValue =
        assetClassValue
          (psToken $ lsPlatformSettings logicSettings)
          (jPrice judge)

      deadline :: POSIXTime
      deadline = aTime accusation + jMaxDuration judge

      validLogic :: Bool
      validLogic =
        txOutValue ownInput `geq` shameToken
          && txOutValue ownOutput `geq` shameToken

      validLogicDatum :: Bool
      validLogicDatum = logicDatum == LSWaitingEnd judge accusation verdict

      validLogicValue :: Bool
      validLogicValue = txOutValue ownOutput == (txOutValue ownInput <> negate judgeValue)

      signedByJudge :: Bool
      signedByJudge = txSignedBy info (jPubKeyHash judge)

      validVerdict :: Bool
      validVerdict = all (`Map.member` lsInputs logicSettings) (Map.keys verdict)
mkLogicValidator
  logicSettings
  (LSWaitingEnd judge accusation verdict)
  LRConsume
  ctx =
    traceIfFalse "Logic Consume - Invalid logic" validLogic
      && traceIfFalse "Logic Consume - Logic UTxO did not disappear" logicDisappeared
      && traceIfFalse "Logic Consume - Invalid contract" validContract
      && traceIfFalse "Logic Consume - Invalid contract value" validContractValue
      && traceIfFalse "Logic Consume - Invalid tokens distribution" validDistribution
      && traceIfFalse "Logic Consume - Invalid accused account" validAccount
      && traceIfFalse "Logic Consume - Accused account did not receive shame token" validAccountValue
      && traceIfFalse "Logic Consume - Invalid accused account datum" validAccountDatum
      && traceIfFalse "Logic Consume - You need to wait" validTime
    where
      info :: TxInfo
      info = scriptContextTxInfo ctx

      ownInput :: TxOut
      ownInput = case findOwnInput ctx of
        Nothing -> traceError "Logic Consume - Logic input missing"
        Just i -> txInInfoResolved i

      shameToken :: Value
      shameToken = assetClassValue (psShameToken $ lsPlatformSettings logicSettings) 1

      contractInput, contractOutput :: TxOut
      contractInput = case strictFindInputWithValHash (fst $ lsContract logicSettings) info of
        Just o -> o
        Nothing -> traceError "Logic Consume - Couldn't find an unique contract input"
      contractOutput = case strictFindOutputWithValHash (fst $ lsContract logicSettings) info of
        Just o -> o
        Nothing -> traceError "Logic Consume - Couldn't find an unique contract output"

      contractDatum :: ContractDatum
      contractDatum = case findContractDatum contractInput (`findDatum` info) of
        Just dat -> dat
        Nothing -> traceError "Logic Consume - Contract Datum could not be found"

      contractNFT :: Value
      contractNFT = assetClassValue (snd $ lsContract logicSettings) 1

      sigs :: [Sig]
      sigs = findSignatures (lsSignatureSymbol logicSettings) (txOutValue ownInput)

      judgeSig :: Sig
      judgeSig = case find (\s -> sUser s == jPubKeyHash judge) sigs of
        Just s -> s
        Nothing -> traceError "Logic Consume - Judge sig not found"

      accuserSig :: Sig
      accuserSig = case find (\s -> sUser s == aAccuser accusation) sigs of
        Just s -> s
        Nothing -> traceError "Logic Consume - Accuser sig not found"

      accusedSig :: Sig
      accusedSig = case find (\s -> sUser s == aAccused accusation) sigs of
        Just s -> s
        Nothing -> traceError "Logic Consume - Accused sig not found"

      accountValHash :: ValidatorHash
      accountValHash = sScript judgeSig

      judgeSigValue :: Value
      judgeSigValue =
        signatureValue'
          (lsSignatureSymbol logicSettings)
          judgeSig

      accuserSigValue :: Value
      accuserSigValue =
        signatureValue'
          (lsSignatureSymbol logicSettings)
          accuserSig

      accusedSigValue :: Value
      accusedSigValue =
        signatureValue'
          (lsSignatureSymbol logicSettings)
          accusedSig

      accountInput, accountOutput :: Maybe TxOut
      accountInput = strictFindInputWithValHash accountValHash info
      accountOutput = strictFindOutputWithValHash accountValHash info

      inputAccountDatum, outputAccountDatum :: Maybe AccountDatum
      inputAccountDatum = do
        acc <- accountInput
        findAccountDatum acc (`findDatum` info)
      outputAccountDatum = do
        acc <- accountOutput
        findAccountDatum acc (`findDatum` info)

      trust :: Integer
      trust = sTrust $ cdService contractDatum

      trustValue :: Value
      trustValue =
        assetClassValue
          (psToken $ lsPlatformSettings logicSettings)
          trust

      guilty :: Bool
      guilty =
        isGuilty
          (lsLogic logicSettings)
          (lsInputs logicSettings)
          verdict

      expectedContractValueDifference :: Value
      expectedContractValueDifference =
        if guilty
          then
            accuserSigValue
              <> judgeSigValue
              <> trustValue
          else
            accuserSigValue
              <> accusedSigValue
              <> judgeSigValue
              <> trustValue
              <> trustValue

      validGuiltyDistribution :: Proportion -> Bool
      validGuiltyDistribution p =
        paidToAccuser == fst (trustProportion p trust)
          && paidToAccused == snd (trustProportion p trust)
          && paidToJudge == jPrice judge
        where
          paidToAccuser :: Integer
          paidToAccuser =
            assetClassValueOf
              (valuePaidTo info (aAccuser accusation))
              (psToken $ lsPlatformSettings logicSettings)

          paidToAccused :: Integer
          paidToAccused =
            assetClassValueOf
              (valuePaidTo info (aAccused accusation))
              (psToken $ lsPlatformSettings logicSettings)

          paidToJudge :: Integer
          paidToJudge =
            assetClassValueOf
              (valuePaidTo info (jPubKeyHash judge))
              (psToken $ lsPlatformSettings logicSettings)

      validInnocentDistribution :: Bool
      validInnocentDistribution = paidToJudge == jPrice judge
        where
          paidToJudge :: Integer
          paidToJudge =
            assetClassValueOf
              (valuePaidTo info (jPubKeyHash judge))
              (psToken $ lsPlatformSettings logicSettings)

      validLogic :: Bool
      validLogic = txOutValue ownInput `geq` shameToken

      logicDisappeared :: Bool
      logicDisappeared = null (getContinuingOutputs ctx)

      validContract :: Bool
      validContract =
        txOutValue contractInput `geq` contractNFT
          && txOutValue contractOutput `geq` contractNFT

      validContractValue :: Bool
      validContractValue =
        txOutValue contractOutput
          <> negate (txOutValue contractInput)
          == expectedContractValueDifference

      validDistribution :: Bool
      validDistribution =
        maybe
          validInnocentDistribution
          validGuiltyDistribution
          ( failedProportion
              (lsInputs logicSettings)
              verdict
              (lsLogic logicSettings)
          )

      validAccount :: Bool
      validAccount = case (accountInput, accountOutput) of
        (Just ai, Just ao) ->
          txOutValue ai `geq` accusedSigValue
            && txOutValue ao `geq` accusedSigValue
        _ ->
          not guilty || traceError "Logic Consume - Could not find accused account"

      validAccountValue :: Bool
      validAccountValue = case (accountInput, accountOutput) of
        (Just ai, Just ao) ->
          txOutValue ao <> negate (txOutValue ai)
            == accusedSigValue
        _ -> not guilty || traceError "Logic Consume - Could not find accused account"

      validAccountDatum :: Bool
      validAccountDatum = case (inputAccountDatum, outputAccountDatum) of
        (Just iDt, Just oDt) -> oDt == declaredGuiltyCAS iDt
        _ -> not guilty || traceError "Logic Consume - Could not find accused account datum"

      deadline :: POSIXTime
      deadline = aTime accusation + jMaxDuration judge + jMaxDuration judge

      validTime :: Bool
      validTime = not $ to deadline `contains` txInfoValidRange info

data LogicType

instance Scripts.ValidatorTypes LogicType where
  type DatumType LogicType = LogicState
  type RedeemerType LogicType = LogicRedeemer

typedLogicValidator :: LogicSettings -> Scripts.TypedValidator LogicType
typedLogicValidator logicSettings =
  Scripts.mkTypedValidator @LogicType
    ( $$(PlutusTx.compile [||mkLogicValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode logicSettings
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @LogicState @LogicRedeemer

logicValidator :: LogicSettings -> Validator
logicValidator logicSettings = Scripts.validatorScript $ typedLogicValidator logicSettings

logicValHash :: LogicSettings -> Ledger.ValidatorHash
logicValHash logicSettings = validatorHash (logicValidator logicSettings)
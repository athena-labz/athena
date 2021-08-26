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

module Membership.OnChain.Account where

import Ledger
    ( PubKeyHash,
      ValidatorHash,
      TxOut(txOutValue),
      Address,
      scriptAddress,
      validatorHash,
      findDatum,
      ownHash,
      txSignedBy,
      ScriptContext(scriptContextTxInfo),
      TxInfo,
      Validator )
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value
    ( Value, AssetClass, assetClassValue, assetClassValueOf, geq )
import Membership.Account
    ( AccountDatum(adReviewCredit),
      AccountRedeemer(..),
      AccountType,
      applyCAS,
      findAccountDatum,
      strictFindOutAndIn,
      findOutAndIn,
      sigTokenIn )
import Membership.PlatformSettings (PlatformSettings (..))
import qualified PlutusTx
import PlutusTx.Prelude
    ( return,
      Bool(..),
      Integer,
      Maybe(..),
      Eq((==)),
      (.),
      (&&),
      any,
      length,
      negate,
      ($),
      fst,
      snd,
      traceError,
      traceIfFalse,
      AdditiveGroup((-)),
      Ord((>=)) )
import Wallet.Emulator.Wallet ()
import Prelude (Semigroup (..))
import Membership.Contract
    ( findContractDatum,
      isInitial,
      strictFindContractInputWithValHash',
      strictFindContractOutputWithValHash',
      ContractDatum )
import Membership.Signature
    ( findSignature,
      findSignatures,
      signatureAssetClass',
      signatureValue' )

{-# INLINEABLE validateCreate #-}
validateCreate :: PlatformSettings -> AccountDatum -> ScriptContext -> Bool
validateCreate ps dat ctx = case owner of
  (Just pkh) ->
    traceIfFalse "SIG token missing" (sigTokenIn (sigToken pkh) ctx)
      && traceIfFalse "transaction not signed by account owner" (txSignedBy info pkh)
      && traceIfFalse "SIG token was not paid to the contract" (sigPaid pkh)
      && traceIfFalse "unexpected output value" valueMatches
      && traceIfFalse "wrong contract datum" validContractDatum
      && traceIfFalse "wrong account datum" validAccountDatum
  Nothing -> traceError "invalid account"
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    inputDatum, outputDatum :: Maybe AccountDatum
    inputDatum = findAccountDatum ownInput (`findDatum` info)
    outputDatum = findAccountDatum ownOutput (`findDatum` info)

    owner :: Maybe PubKeyHash
    owner = findSignature (psSignatureSymbol ps) (txOutValue ownInput)

    sigToken :: PubKeyHash -> AssetClass
    sigToken pkh = signatureAssetClass' ps pkh (ownHash ctx)

    contractOutput :: Maybe (ContractDatum, Value)
    contractOutput = do
      o <- strictFindContractOutputWithValHash' ps info
      d <- findContractDatum o (`findDatum` info)
      return (d, txOutValue o)

    sigPaid :: PubKeyHash -> Bool
    sigPaid pkh = case contractOutput of
      Just (_, val) -> assetClassValueOf val (sigToken pkh) == 1
      _ -> False

    dsetProfit :: Integer
    dsetProfit =
      assetClassValueOf
        (txOutValue ownOutput)
        (psToken ps)
        - assetClassValueOf (txOutValue ownInput) (psToken ps)

    valueMatches :: Bool
    valueMatches = dsetProfit >= psTxFee ps

    validContractDatum :: Bool
    validContractDatum = case contractOutput of
      Just (d, _) -> isInitial d
      Nothing -> False

    validAccountDatum :: Bool
    validAccountDatum = case (inputDatum, outputDatum) of
      (Just iDat, Just oDat) -> (iDat == dat) && applyCAS iDat == oDat
      _ -> traceError "could not find input or output datum"

{-# INLINEABLE validateSign #-}
validateSign :: PlatformSettings -> AccountDatum -> ScriptContext -> Bool
validateSign ps dat ctx = case owner of
  (Just pkh) ->
    traceIfFalse "SIG token missing" (sigTokenIn (sigToken pkh) ctx)
      && traceIfFalse "transaction not signed by account owner" (txSignedBy info pkh)
      && traceIfFalse "SIG token was not paid to the contract" (sigPaid pkh)
      && traceIfFalse "final value does not match" (valueMatches pkh)
      && traceIfFalse "wrong contract datum" validContractDatum
      && traceIfFalse "wrong account datum" validAccountDatum
  Nothing -> traceError "invalid account"
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    inputDatum, outputDatum :: Maybe AccountDatum
    inputDatum = findAccountDatum ownInput (`findDatum` info)
    outputDatum = findAccountDatum ownOutput (`findDatum` info)

    owner :: Maybe PubKeyHash
    owner = findSignature (psSignatureSymbol ps) (txOutValue ownInput)

    fees :: Value
    fees = assetClassValue (psToken ps) (psTxFee ps)

    sigToken :: PubKeyHash -> AssetClass
    sigToken pkh = signatureAssetClass' ps pkh (ownHash ctx)

    contractInput :: Maybe (ContractDatum, Value)
    contractInput = do
      o <- strictFindContractInputWithValHash' ps info
      d <- findContractDatum o (`findDatum` info)
      return (d, txOutValue o)

    contractOutput :: Maybe (ContractDatum, Value)
    contractOutput = do
      o <- strictFindContractOutputWithValHash' ps info
      d <- findContractDatum o (`findDatum` info)
      return (d, txOutValue o)

    sigPaid :: PubKeyHash -> Bool
    sigPaid pkh = case contractOutput of
      Just (_, val) -> assetClassValueOf val (sigToken pkh) == 1
      _ -> False

    valueMatches :: PubKeyHash -> Bool
    valueMatches pkh =
      txOutValue ownOutput
        == ( txOutValue ownInput
               <> fees
               <> negate (signatureValue' ps pkh (ownHash ctx))
           )

    validContractDatum :: Bool
    validContractDatum = case (contractInput, contractOutput) of
      (Just (iDat, _), Just (oDat, _)) -> iDat == oDat
      _ -> False

    validAccountDatum :: Bool
    validAccountDatum = case (inputDatum, outputDatum) of
      (Just iDat, Just oDat) -> (iDat == dat) && applyCAS iDat == oDat
      _ -> False

{-# INLINEABLE validateCollect #-}
validateCollect :: PlatformSettings -> PubKeyHash -> ScriptContext -> Bool
validateCollect ps pkh ctx =
  traceIfFalse "user is not allowed to collect fees" signedByCollector
    && traceIfFalse "invalid datum" (inputDatum == outputDatum)
    && traceIfFalse "invalid output" validOutput
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput, ownOutput :: TxOut
    ownInput = fst $ findOutAndIn pkh ps ctx
    ownOutput = snd $ findOutAndIn pkh ps ctx

    inputDatum, outputDatum :: Maybe AccountDatum
    inputDatum = findAccountDatum ownInput (`findDatum` info)
    outputDatum = findAccountDatum ownOutput (`findDatum` info)

    signedByCollector :: Bool
    signedByCollector = any (txSignedBy info) (psCollectors ps)

    inputValue :: Value
    inputValue = txOutValue ownInput

    inputHasToken :: Bool
    inputHasToken = length (findSignatures (psSignatureSymbol ps) inputValue) == 1

    outputValue :: Value
    outputValue = txOutValue ownOutput

    dsetInput :: Integer
    dsetInput = assetClassValueOf (txOutValue ownInput) (psToken ps)

    validOutput :: Bool
    validOutput = case (inputDatum, inputHasToken) of
      (Just dat, True) -> outputValue `geq` (
        inputValue <> negate (assetClassValue (psToken ps) (dsetInput - adReviewCredit dat)))
      _ -> False

{-# INLINEABLE mkAccountValidator #-}
mkAccountValidator :: PlatformSettings -> AccountDatum -> AccountRedeemer -> ScriptContext -> Bool
mkAccountValidator ps dat Create ctx = validateCreate ps dat ctx
mkAccountValidator ps dat Sign ctx = validateSign ps dat ctx
mkAccountValidator ps _ (Collect pkh) ctx = validateCollect ps pkh ctx
mkAccountValidator _ _ _ _ = False

typedAccountValidator :: PlatformSettings -> Scripts.TypedValidator AccountType
typedAccountValidator ps =
  Scripts.mkTypedValidator @AccountType
    ($$(PlutusTx.compile [||mkAccountValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode ps)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @AccountDatum @AccountRedeemer

accountValidator :: PlatformSettings -> Validator
accountValidator = Scripts.validatorScript . typedAccountValidator

accountValidatorHash :: PlatformSettings -> ValidatorHash
accountValidatorHash = validatorHash . accountValidator

accountAddress :: PlatformSettings -> Ledger.Address
accountAddress = scriptAddress . accountValidator
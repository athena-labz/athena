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
  ( Address,
    PubKeyHash,
    ScriptContext (scriptContextTxInfo),
    TxInfo,
    TxOut (txOutValue),
    Validator,
    ValidatorHash,
    findDatum,
    ownHash,
    scriptAddress,
    txSignedBy,
    validatorHash,
  )
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value
  ( AssetClass,
    Value,
    assetClassValue,
    assetClassValueOf,
    geq,
  )
import Membership.Account
  ( AccountDatum (adReviewCredit),
    AccountRedeemer (..),
    AccountType,
    contractCreationCAS,
    findAccountDatum,
  )
import Membership.Contract
    ( strictFindContractOutputWithValHash,
      findContractDatum,
      isInitial,
      ContractDatum )
import Membership.OnChain.Utils
    ( sigTokenIn, findOutAndIn, strictFindOutAndIn )
import Membership.PlatformSettings (AccountSettings(..), PlatformSettings (..))
import Membership.Signature
    ( findSignatories,
      findSignatory,
      signatureAssetClass,
      signatureValue )
import qualified PlutusTx
import PlutusTx.Prelude
  ( AdditiveGroup ((-)),
    Bool (..),
    Eq ((==)),
    Integer,
    Maybe (..),
    Ord ((>=)),
    any,
    fst,
    length,
    negate,
    return,
    snd,
    traceError,
    traceIfFalse,
    ($),
    (&&),
    (.),
  )
import Prelude (Semigroup (..))

{-# INLINEABLE validateCreate #-}
validateCreate :: AccountSettings -> AccountDatum -> ScriptContext -> Bool
validateCreate (AccountSettings ps sigSym contrValHash _) dat ctx = case owner of
  (Just pkh) ->
    traceIfFalse "SIG token missing" (sigTokenIn (sigToken pkh) ctx)
      && traceIfFalse "transaction not signed by account owner" (txSignedBy info pkh)
      && traceIfFalse "SIG token was not paid to the contract" (sigPaid pkh)
      && traceIfFalse "unexpected output value" valueMatches
      && traceIfFalse "wrong contract datum" (validContractDatum pkh)
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
    owner = findSignatory sigSym (txOutValue ownInput)

    sigToken :: PubKeyHash -> AssetClass
    sigToken pkh = signatureAssetClass sigSym pkh (ownHash ctx)

    contractOutput :: Maybe (ContractDatum, Value)
    contractOutput = do
      o <- strictFindContractOutputWithValHash contrValHash info
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

    validContractDatum :: PubKeyHash -> Bool
    validContractDatum pkh = case contractOutput of
      Just (d, _) -> isInitial pkh d
      Nothing -> False

    validAccountDatum :: Bool
    validAccountDatum = case (inputDatum, outputDatum) of
      (Just iDat, Just oDat) -> (iDat == dat) && contractCreationCAS iDat == oDat
      _ -> traceError "could not find input or output datum"

{-# INLINEABLE validateSign #-}
validateSign :: AccountSettings -> AccountDatum -> ScriptContext -> Bool
validateSign (AccountSettings ps sigSym contrValHash _) dat ctx = case owner of
  (Just pkh) ->
    traceIfFalse "Account - SIG token missing" (sigTokenIn (sigToken pkh) ctx)
      && traceIfFalse "Account - Transaction not signed by account owner" (txSignedBy info pkh)
      && traceIfFalse "Account - SIG token was not paid to the contract" (sigPaid pkh)
      && traceIfFalse "Account - final value does not match" (valueMatches pkh)
      && traceIfFalse "Account - wrong account datum" validAccountDatum
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
    owner = findSignatory sigSym (txOutValue ownInput)

    fees :: Value
    fees = assetClassValue (psToken ps) (psTxFee ps)

    sigToken :: PubKeyHash -> AssetClass
    sigToken pkh = signatureAssetClass sigSym pkh (ownHash ctx)

    contractOutput :: Maybe (ContractDatum, Value)
    contractOutput = do
      o <- strictFindContractOutputWithValHash contrValHash info
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
               <> negate (signatureValue sigSym pkh (ownHash ctx))
           )

    validAccountDatum :: Bool
    validAccountDatum = case (inputDatum, outputDatum) of
      (Just iDat, Just oDat) -> (iDat == dat) && contractCreationCAS iDat == oDat
      _ -> False

{-# INLINEABLE validateCollect #-}
validateCollect :: AccountSettings -> PubKeyHash -> ScriptContext -> Bool
validateCollect (AccountSettings ps sigSym _ collectors) pkh ctx =
  traceIfFalse "user is not allowed to collect fees" signedByCollector
    && traceIfFalse "invalid datum" (inputDatum == outputDatum)
    && traceIfFalse "invalid output" validOutput
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput, ownOutput :: TxOut
    ownInput = fst $ findOutAndIn pkh sigSym ctx
    ownOutput = snd $ findOutAndIn pkh sigSym ctx

    inputDatum, outputDatum :: Maybe AccountDatum
    inputDatum = findAccountDatum ownInput (`findDatum` info)
    outputDatum = findAccountDatum ownOutput (`findDatum` info)

    signedByCollector :: Bool
    signedByCollector = any (txSignedBy info) collectors

    inputValue :: Value
    inputValue = txOutValue ownInput

    inputHasToken :: Bool
    inputHasToken = length (findSignatories sigSym inputValue) == 1

    outputValue :: Value
    outputValue = txOutValue ownOutput

    dsetInput :: Integer
    dsetInput = assetClassValueOf (txOutValue ownInput) (psToken ps)

    validOutput :: Bool
    validOutput = case (inputDatum, inputHasToken) of
      (Just dat, True) ->
        outputValue
          `geq` ( inputValue <> negate (assetClassValue (psToken ps) (dsetInput - adReviewCredit dat))
                )
      _ -> False

{-# INLINEABLE mkAccountValidator #-}
mkAccountValidator ::
  AccountSettings ->
  AccountDatum ->
  AccountRedeemer ->
  ScriptContext ->
  Bool

mkAccountValidator as dat ACreate ctx = validateCreate as dat ctx
mkAccountValidator as dat ASign ctx = validateSign as dat ctx
mkAccountValidator as _ (ACollect pkh) ctx = validateCollect as pkh ctx

typedAccountValidator :: AccountSettings -> Scripts.TypedValidator AccountType
typedAccountValidator as =
  Scripts.mkTypedValidator @AccountType
    ($$(PlutusTx.compile [||mkAccountValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode as)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @AccountDatum @AccountRedeemer

accountValidator :: AccountSettings -> Validator
accountValidator = Scripts.validatorScript . typedAccountValidator

accountValidatorHash :: AccountSettings -> ValidatorHash
accountValidatorHash = validatorHash . accountValidator

accountAddress :: AccountSettings -> Ledger.Address
accountAddress = scriptAddress . accountValidator
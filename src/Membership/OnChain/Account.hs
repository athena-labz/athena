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

import Control.Monad hiding (fmap)
import qualified Data.Map as Map
import Data.Monoid (Last (..), Monoid (mempty))
import Data.Monoid as M
import Data.Text (Text, pack)
import Ledger hiding (singleton)
import Ledger.Ada as Ada (Ada (getLovelace), fromValue)
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value
import Membership.AccountDatum
import Membership.AccountRedeemer (AccountRedeemer (..))
import Membership.ContractDatum
import Membership.PlatformSettings (PlatformSettings (..))
import Membership.Sample as S
import Membership.Utils
import Plutus.Contract as Contract
import Plutus.Contract.StateMachine
import qualified PlutusTx
import PlutusTx.Prelude hiding (ByteString, Semigroup (..), check, unless)
import Wallet.Emulator.Wallet ()
import Prelude (Semigroup (..), Show (..), String)

{-# INLINEABLE findContractDatum #-}
findContractDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe ContractDatum
findContractDatum o f = do
  dh <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromBuiltinData d

{-# INLINEABLE findAccountDatum #-}
findAccountDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe AccountDatum
findAccountDatum o f = do
  dh <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromBuiltinData d

{-# INLINEABLE findOutAndIn #-}
findOutAndIn :: ScriptContext -> (TxOut, TxOut)
findOutAndIn ctx = (ownInput, ownOutput)
  where
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "account input missing"
      Just i -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "expected exactly one account output"

{-# INLINEABLE sigTokenIn #-}
sigTokenIn :: AssetClass -> ScriptContext -> Bool
sigTokenIn sig ctx = inputHasToken && outputHasToken
  where
    ownInput, ownOutput :: TxOut
    ownInput = fst $ findOutAndIn ctx
    ownOutput = snd $ findOutAndIn ctx

    inputTokens :: Integer
    inputTokens = assetClassValueOf (txOutValue ownInput) sig

    inputHasToken :: Bool
    inputHasToken = inputTokens > 0

    outputTokens :: Integer
    outputTokens = assetClassValueOf (txOutValue ownOutput) sig

    outputHasToken :: Bool
    outputHasToken = ((inputTokens - outputTokens) == 1) && (outputTokens > 0)

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
    ownInput = fst $ findOutAndIn ctx
    ownOutput = snd $ findOutAndIn ctx

    inputDatum, outputDatum :: Maybe AccountDatum
    inputDatum = findAccountDatum ownInput (`findDatum` info)
    outputDatum = findAccountDatum ownOutput (`findDatum` info)

    owner :: Maybe PubKeyHash
    owner = findSignature (psSignatureSymbol ps) (txOutValue ownInput)

    fees :: Value
    fees = assetClassValue (psToken ps) (psTxFee ps)

    sigToken :: PubKeyHash -> AssetClass
    sigToken = signatureAssetClass ps

    contractOutput :: Maybe (ContractDatum, Value)
    contractOutput = do
      o <- strictFindOutputWithValHash ps info
      dat <- findContractDatum o (`findDatum` info)
      return (dat, txOutValue o)

    sigPaid :: PubKeyHash -> Bool
    sigPaid pkh = case contractOutput of
      Just (_, val) -> assetClassValueOf val (sigToken pkh) == 1
      _ -> False

    expectedValue :: PubKeyHash -> Value
    expectedValue pkh =
      txOutValue ownInput
        <> fees
        <> negate (signatureValue ps pkh)

    dsetProfit :: Integer
    dsetProfit =
      assetClassValueOf (txOutValue ownOutput) (psToken ps) - assetClassValueOf (txOutValue ownInput) (psToken ps)

    valueMatches :: Bool
    valueMatches = dsetProfit >= psTxFee ps

    validContractDatum :: Bool
    validContractDatum = case contractOutput of
      Just (dat, _) -> isInitial dat
      Nothing -> False

    validAccountDatum :: Bool
    validAccountDatum = case (inputDatum, outputDatum) of
      (Just iDat, Just oDat) -> applyCAS iDat == oDat
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
    ownInput = fst $ findOutAndIn ctx
    ownOutput = snd $ findOutAndIn ctx

    inputDatum, outputDatum :: Maybe AccountDatum
    inputDatum = findAccountDatum ownInput (`findDatum` info)
    outputDatum = findAccountDatum ownOutput (`findDatum` info)

    owner :: Maybe PubKeyHash
    owner = findSignature (psSignatureSymbol ps) (txOutValue ownInput)

    fees :: Value
    fees = assetClassValue (psToken ps) (psTxFee ps)

    sigToken :: PubKeyHash -> AssetClass
    sigToken = signatureAssetClass ps

    contractInput :: Maybe (ContractDatum, Value)
    contractInput = do
      o <- strictFindInputWithValHash ps info
      dat <- findContractDatum o (`findDatum` info)
      return (dat, txOutValue o)

    contractOutput :: Maybe (ContractDatum, Value)
    contractOutput = do
      o <- strictFindOutputWithValHash ps info
      dat <- findContractDatum o (`findDatum` info)
      return (dat, txOutValue o)

    sigPaid :: PubKeyHash -> Bool
    sigPaid pkh = case contractOutput of
      Just (_, val) -> assetClassValueOf val (sigToken pkh) == 1
      _ -> False

    valueMatches :: PubKeyHash -> Bool
    valueMatches pkh =
      txOutValue ownOutput
        == ( txOutValue ownInput
               <> fees
               <> negate (signatureValue ps pkh)
           )

    validContractDatum :: Bool
    validContractDatum = case (contractInput, contractOutput) of
      (Just (iDat, _), Just (oDat, _)) -> iDat == oDat
      _ -> False

    validAccountDatum :: Bool
    validAccountDatum = case (inputDatum, outputDatum) of
      (Just iDat, Just oDat) -> applyCAS iDat == oDat
      _ -> False

{-# INLINEABLE validateCollect #-}
validateCollect :: PlatformSettings -> ScriptContext -> Bool
validateCollect ps ctx =
  traceIfFalse "user is not allowed to collect fees" signedByCollector
    && traceIfFalse "invalid datum" (inputDatum == outputDatum)
    && traceIfFalse "invalid output" validOutput
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    sigToken :: PubKeyHash -> AssetClass
    sigToken = signatureAssetClass ps

    signedByCollector :: Bool
    signedByCollector = any (txSignedBy info) (psCollectors ps)

    ownInput, ownOutput :: TxOut
    ownInput = fst $ findOutAndIn ctx
    ownOutput = snd $ findOutAndIn ctx

    inputDatum, outputDatum :: Maybe AccountDatum
    inputDatum = findAccountDatum ownInput (`findDatum` info)
    outputDatum = findAccountDatum ownOutput (`findDatum` info)

    inputValue :: Value
    inputValue = txOutValue ownInput

    inputHasToken :: Bool
    inputHasToken = length (findSignatures (psSignatureSymbol ps) inputValue) == 1

    outputValue :: Value
    outputValue = txOutValue ownOutput

    validOutput :: Bool
    validOutput = case (inputDatum, inputHasToken) of
      (Just dat, True) -> outputValue `geq` (inputValue <> negate (reviewCreditValue ps dat))
      _ -> False

{-# INLINEABLE mkAccountValidator #-}
mkAccountValidator :: PlatformSettings -> AccountDatum -> AccountRedeemer -> ScriptContext -> Bool
mkAccountValidator ps dat Create ctx = validateCreate ps dat ctx
mkAccountValidator ps dat Sign ctx = validateSign ps dat ctx
mkAccountValidator ps dat Collect ctx = validateCollect ps ctx
mkAccountValidator _ _ _ _ = False

data AccountType

instance Scripts.ValidatorTypes AccountType where
  type DatumType AccountType = AccountDatum
  type RedeemerType AccountType = AccountRedeemer

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
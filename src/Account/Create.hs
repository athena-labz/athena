{-# LANGUAGE BangPatterns #-}
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

module Account.Create where

import Account
import Ledger hiding (singleton)
import Ledger.Typed.Scripts as Scripts
import Ledger.Value
import qualified PlutusTx
import PlutusTx.Prelude (BuiltinString, Bool (..), Maybe (..), (==), (.),
                         ($), (&&), (<>), traceError, traceIfFalse)
import Utils

{-# INLINEABLE sigPolicyTraceIfFalse #-}
sigPolicyTraceIfFalse :: BuiltinString -> Bool -> Bool
sigPolicyTraceIfFalse msg = traceIfFalse ("Sig Policy - " <> msg)

{-# INLINEABLE sigPolicyTraceError #-}
sigPolicyTraceError :: forall a. BuiltinString -> a
sigPolicyTraceError msg = traceError ("Sig Policy - " <> msg)

-- The policy that will allow or not the minting of new signature tokens
-- Signature tokens are DigiService's way of authenticating accounts,
-- making sure the fees are paid and identifying users
{-# INLINEABLE mkSignaturePolicy #-}
mkSignaturePolicy :: AccountSettings -> PubKeyHash -> ScriptContext -> Bool
mkSignaturePolicy sett pkh ctx =
  traceIfFalse "invalid signature" (txSignedBy info pkh)
    && traceIfFalse "invalid minting" validMinting
    && traceIfFalse "invalid account value" validAccountValue
    && traceIfFalse "invalid account datum" validAccountDatum
  where
    -- The basic information about this transaction
    info :: TxInfo
    info = scriptContextTxInfo ctx

    expectedTokenName :: TokenName
    expectedTokenName = parsePubKeyHash pkh

    validMinting :: Bool
    validMinting = case flattenValue (txInfoMint info) of
      [(cs, tn, amt)] ->
        cs == ownCurrencySymbol ctx && tn == expectedTokenName && amt == 100

    sigValue :: Value
    sigValue = singleton (ownCurrencySymbol ctx) expectedTokenName 100

    -- The entrance fee required to enter in the platform
    fees :: Value
    fees = assetClassValue (casToken sett) (casEntranceFee sett)

    accountOutput :: TxOut
    accountOutput = case strictFindOutputWithValHash (casAccValHash sett) info of
      Just o -> o

    accountDatum :: AccountDatum
    accountDatum = case findAccountDatum accountOutput (`findDatum` info) of
      Just dat -> dat

    validAccountValue :: Bool
    validAccountValue = txOutValue accountOutput == sigValue <> fees

    init :: AccountDatum
    init = initDatum (ownCurrencySymbol ctx) (casTickets sett)

    validAccountDatum :: Bool
    validAccountDatum =
      accountDatum == initDatum (ownCurrencySymbol ctx) (casTickets sett)

signaturePolicy :: AccountSettings -> Scripts.MintingPolicy
signaturePolicy sett =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkSignaturePolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode sett

signatureCurrencySymbol :: AccountSettings -> CurrencySymbol
signatureCurrencySymbol = scriptCurrencySymbol . signaturePolicy

signaturePlutusScript :: AccountSettings -> Script
signaturePlutusScript = unMintingPolicyScript . signaturePolicy

signatureValidator :: AccountSettings -> Validator
signatureValidator = Validator . signaturePlutusScript

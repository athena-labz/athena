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
mkSignaturePolicy sett pkh ctx = True

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

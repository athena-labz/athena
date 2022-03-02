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

module Contract.Mediate where

import Account
import Contract
import Ledger hiding (singleton)
import Ledger.Scripts
import Ledger.Typed.Scripts as Scripts
import Ledger.Value
import Plutus.ChainIndex
import qualified PlutusTx
import qualified PlutusTx.AssocMap as PlutusMap
import PlutusTx.Prelude
import qualified PlutusTx.Ratio as R
import Utils
import qualified Prelude

{-# INLINEABLE ticketName #-}
ticketName :: BuiltinByteString
ticketName = "resolve-dispute"

{-# INLINEABLE mkResolveDisputePolicy #-}
mkResolveDisputePolicy ::
  ContractSettings ->
  (PubKeyHash, BuiltinByteString, POSIXTime) ->
  ScriptContext ->
  Bool
mkResolveDisputePolicy sett (pkh, vdt, dln) ctx = True

resolveDisputePolicy :: ContractSettings -> Scripts.MintingPolicy
resolveDisputePolicy sett =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkResolveDisputePolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode sett

resolveDisputeCurrencySymbol :: ContractSettings -> CurrencySymbol
resolveDisputeCurrencySymbol = scriptCurrencySymbol . resolveDisputePolicy

resolveDisputePlutusScript :: ContractSettings -> Script
resolveDisputePlutusScript = unMintingPolicyScript . resolveDisputePolicy

resolveDisputeValidator :: ContractSettings -> Validator
resolveDisputeValidator = Validator . resolveDisputePlutusScript

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

module Contract.Accuse where

import Account
import Contract
import Data.Aeson hiding (Value)
import GHC.Generics
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
ticketName = "raise-dispute"

{-# INLINEABLE mkRaiseDisputePolicy #-}
mkRaiseDisputePolicy ::
  ContractSettings ->
  (PubKeyHash, PubKeyHash, POSIXTime, POSIXTime) ->
  ScriptContext ->
  Bool
mkRaiseDisputePolicy sett (pkh, acd, time, dln) ctx = True

raiseDisputePolicy :: ContractSettings -> Scripts.MintingPolicy
raiseDisputePolicy sett =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkRaiseDisputePolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode sett

raiseDisputeCurrencySymbol :: ContractSettings -> CurrencySymbol
raiseDisputeCurrencySymbol = scriptCurrencySymbol . raiseDisputePolicy

raiseDisputePlutusScript :: ContractSettings -> Script
raiseDisputePlutusScript = unMintingPolicyScript . raiseDisputePolicy

raiseDisputeValidator :: ContractSettings -> Validator
raiseDisputeValidator = Validator . raiseDisputePlutusScript


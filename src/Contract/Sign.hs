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

module Contract.Sign where

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
ticketName = "sign-contract"

{-# INLINEABLE mkSignContractPolicy #-}
mkSignContractPolicy ::
  ContractSettings ->
  (PubKeyHash, Integer, AssetClass) ->
  ScriptContext ->
  Bool
mkSignContractPolicy sett (pkh, role, nft) ctx = True

signContractPolicy :: ContractSettings -> Scripts.MintingPolicy
signContractPolicy sett =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkSignContractPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode sett

signContractCurrencySymbol :: ContractSettings -> CurrencySymbol
signContractCurrencySymbol = scriptCurrencySymbol . signContractPolicy

signContractPlutusScript :: ContractSettings -> Script
signContractPlutusScript = unMintingPolicyScript . signContractPolicy

signContractValidator :: ContractSettings -> Validator
signContractValidator = Validator . signContractPlutusScript


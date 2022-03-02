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

module Contract.Create where

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

{-# INLINABLE ticketName #-}
ticketName :: BuiltinByteString
ticketName = "create-contract"

{-# INLINEABLE mkCreateContractPolicy #-}
mkCreateContractPolicy ::
  ContractSettings ->
  (PubKeyHash, Integer, AssetClass) ->
  ScriptContext ->
  Bool
mkCreateContractPolicy sett (pkh, role, nft) ctx = True

createContractPolicy :: ContractSettings -> Scripts.MintingPolicy
createContractPolicy sett =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkCreateContractPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode sett

createContractCurrencySymbol :: ContractSettings -> CurrencySymbol
createContractCurrencySymbol = scriptCurrencySymbol . createContractPolicy

createContractPlutusScript :: ContractSettings -> Script
createContractPlutusScript = unMintingPolicyScript . createContractPolicy

createContractValidator :: ContractSettings -> Validator
createContractValidator = Validator . createContractPlutusScript

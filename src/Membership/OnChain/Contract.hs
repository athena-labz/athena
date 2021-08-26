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

module Membership.OnChain.Contract where

import Ledger (ScriptContext, ValidatorHash)
import Ledger.Scripts (Validator, validatorHash)
import qualified Ledger.Typed.Scripts as Scripts
import Membership.Contract (ContractDatum)
import qualified PlutusTx
import PlutusTx.Prelude (Bool (..), Integer, (.))

{-# INLINEABLE mkContractValidator #-}
mkContractValidator :: Integer -> ContractDatum -> () -> ScriptContext -> Bool
mkContractValidator _ _ _ _ = True

typedValidator :: Integer -> Scripts.TypedValidator ContractType
typedValidator i =
  Scripts.mkTypedValidator @ContractType
    ($$(PlutusTx.compile [||mkContractValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode i)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @ContractDatum @()


data ContractType
instance Scripts.ValidatorTypes ContractType where
    type instance DatumType ContractType = ContractDatum
    type instance RedeemerType ContractType = ()

validator :: Integer -> Validator
validator = Scripts.validatorScript . typedValidator

valHash :: Integer -> Ledger.ValidatorHash
valHash = validatorHash . validator

sampleTypedValidator :: Scripts.TypedValidator ContractType
sampleTypedValidator = typedValidator 694

sampleContractVal :: Validator
sampleContractVal = validator 694

sampleContractHash :: ValidatorHash
sampleContractHash = valHash 694
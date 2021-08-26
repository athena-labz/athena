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

module Membership.Logic where

import Ledger (ScriptContext, ValidatorHash)
import Ledger.Scripts (Validator, validatorHash)
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import PlutusTx.Prelude (Bool (..), Integer, (.), (&&), (==))

{-# INLINEABLE mkLogicValidator #-}
mkLogicValidator :: Integer -> () -> () -> ScriptContext -> Bool
mkLogicValidator _ _ _ _ = True && (() == ())

typedLogicValidator :: Integer -> Scripts.TypedValidator LogicType
typedLogicValidator i =
  Scripts.mkTypedValidator @LogicType
    ($$(PlutusTx.compile [||mkLogicValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode i)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @() @()


data LogicType
instance Scripts.ValidatorTypes LogicType where
    type instance DatumType LogicType = ()
    type instance RedeemerType LogicType = ()

validator :: Integer -> Validator
validator = Scripts.validatorScript . typedLogicValidator

logicValHash :: Integer -> Ledger.ValidatorHash
logicValHash = validatorHash . validator

sampleTypedValidator :: Scripts.TypedValidator LogicType
sampleTypedValidator = typedLogicValidator 694

sampleLogicVal :: Validator
sampleLogicVal = validator 694

sampleLogicHash :: ValidatorHash
sampleLogicHash = logicValHash 694
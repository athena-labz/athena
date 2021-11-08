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

module Account.Safe.OnChain where

import Control.Monad (void)
import Data.Aeson
import GHC.Generics
import Account
import Ledger
import Ledger.Scripts
import Ledger.Typed.Scripts as Scripts hiding (validatorHash)
import Ledger.Value
import Plutus.ChainIndex
import qualified PlutusTx
import qualified PlutusTx.AssocMap as PlutusMap
import PlutusTx.Prelude
import qualified PlutusTx.Ratio as R
import qualified Prelude

{-# INLINEABLE mkAccountValidator #-}
mkAccountValidator ::
  AccountDatum ->
  PubKeyHash ->
  ScriptContext ->
  Bool
mkAccountValidator inpDat pkh ctx = True

data AccountType

instance Scripts.ValidatorTypes AccountType where
  type DatumType AccountType = AccountDatum
  type RedeemerType AccountType = PubKeyHash

typedAccountValidator :: Scripts.TypedValidator AccountType
typedAccountValidator =
  Scripts.mkTypedValidator @AccountType
    $$(PlutusTx.compile [||mkAccountValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @AccountDatum @PubKeyHash

accountValidator :: Validator
accountValidator = Scripts.validatorScript typedAccountValidator

accountValidatorHash :: ValidatorHash
accountValidatorHash = validatorHash accountValidator

accountAddress :: Ledger.Address
accountAddress = scriptAddress accountValidator
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

import Account
import Ledger
import Ledger.Typed.Scripts as Scripts hiding (validatorHash)
import Ledger.Value
import qualified Prelude
import qualified PlutusTx
import PlutusTx.Prelude

{-# INLINEABLE mkAccountValidator #-}
mkAccountValidator ::
  AccountDatum ->
  CurrencySymbol ->
  ScriptContext ->
  Bool
mkAccountValidator dat tkt ctx =
  traceIfFalse "invalid ticket" validTicket'
    && traceIfFalse "ticket not present" ticketPresent
  where
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Just o -> txInInfoResolved o

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> o

    validTicket' :: Bool
    validTicket' = tkt `elem` (adTickets dat)

    ticketPresent :: Bool
    ticketPresent =
      any
        (\(cs, _, _) -> cs == tkt)
        (flattenValue (txOutValue ownOutput <> negate (txOutValue ownInput)))

data AccountType

instance Scripts.ValidatorTypes AccountType where
  type DatumType AccountType = AccountDatum
  type RedeemerType AccountType = CurrencySymbol

typedAccountValidator :: Scripts.TypedValidator AccountType
typedAccountValidator =
  Scripts.mkTypedValidator @AccountType
    $$(PlutusTx.compile [||mkAccountValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @AccountDatum @CurrencySymbol

accountValidator :: Validator
accountValidator = Scripts.validatorScript typedAccountValidator

accountValidatorHash :: ValidatorHash
accountValidatorHash = validatorHash accountValidator

accountAddress :: Ledger.Address
accountAddress = scriptAddress accountValidator

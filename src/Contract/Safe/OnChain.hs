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

module Contract.Safe.OnChain where

import Control.Monad (void)
import Data.Aeson
import GHC.Generics
import Contract
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

{-# INLINEABLE mkContractValidator #-}
mkContractValidator ::
  ContractDatum ->
  CurrencySymbol ->
  ScriptContext ->
  Bool
mkContractValidator dat tkt ctx =
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
    validTicket' = tkt `elem` (cdTickets dat)

    ticketPresent :: Bool
    ticketPresent =
      any
        (\(cs, _, _) -> cs == tkt)
        (flattenValue (txOutValue ownOutput <> negate (txOutValue ownInput)))

data ContractType

instance Scripts.ValidatorTypes ContractType where
  type DatumType ContractType = ContractDatum
  type RedeemerType ContractType = CurrencySymbol

typedContractValidator :: Scripts.TypedValidator ContractType
typedContractValidator =
  Scripts.mkTypedValidator @ContractType
    $$(PlutusTx.compile [||mkContractValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @ContractDatum @CurrencySymbol

contractValidator :: Validator
contractValidator = Scripts.validatorScript typedContractValidator

contractValidatorHash :: ValidatorHash
contractValidatorHash = validatorHash contractValidator

contractAddress :: Ledger.Address
contractAddress = scriptAddress contractValidator
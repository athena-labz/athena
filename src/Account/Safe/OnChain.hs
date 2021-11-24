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
import Control.Monad (void)
import Data.Aeson
import GHC.Generics
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
  AssetClass ->
  ScriptContext ->
  Bool
mkAccountValidator dat tkt ctx =
  traceIfFalse "Account Safe - Invalid ticket" (validTicket')
    && traceIfFalse "Account Safe - Ticket not present" ticketPresent
  where
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "Account Safe - Input not found"
      Just o -> txInInfoResolved o

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "Account Safe - Own unique output not found"

    validTicket' :: Bool
    validTicket' = tkt `elem` (adTickets dat)

    ticketPresent :: Bool
    ticketPresent =
      ( assetClassValueOf
          (txOutValue ownOutput <> negate (txOutValue ownInput))
          tkt
      )
        == 1

data AccountType

instance Scripts.ValidatorTypes AccountType where
  type DatumType AccountType = AccountDatum
  type RedeemerType AccountType = AssetClass

typedAccountValidator :: Scripts.TypedValidator AccountType
typedAccountValidator =
  Scripts.mkTypedValidator @AccountType
    $$(PlutusTx.compile [||mkAccountValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @AccountDatum @AssetClass

accountValidator :: Validator
accountValidator = Scripts.validatorScript typedAccountValidator

accountValidatorHash :: ValidatorHash
accountValidatorHash = validatorHash accountValidator

accountAddress :: Ledger.Address
accountAddress = scriptAddress accountValidator
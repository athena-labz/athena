{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module MarketHandler where

import Control.Monad ( Monad((>>), (>>=)), void )
import Data.Aeson (FromJSON, ToJSON)
import Data.Map as Map ( null, toList )
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger
    ( scriptAddress,
      txSignedBy,
      pubKeyHash,
      contains,
      from,
      txId,
      Address,
      ScriptContext(scriptContextTxInfo),
      TxInfo(txInfoValidRange),
      PubKeyHash,
      Redeemer(Redeemer),
      Validator,
      ValidatorHash,
      POSIXTime )
import Ledger.Ada as Ada ( lovelaceValueOf )
import Ledger.Constraints as Constraints
    ( otherScript,
      unspentOutputs,
      TxConstraints,
      mustPayToTheScript,
      mustSpendScriptOutput,
      mustValidateIn )
import qualified Ledger.Typed.Scripts as Scripts
import Playground.Contract (ToSchema, ensureKnownCurrencies, printJson, printSchemas, stage)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract
  ( AsContractError,
    Contract,
    Endpoint,
    awaitTxConfirmed,
    currentTime,
    endpoint,
    logInfo,
    select,
    submitTxConstraints,
    submitTxConstraintsWith,
    utxoAt,
    type (.\/),
  )
import qualified Plutus.Contract as Contract
import Plutus.Trace.Emulator
    ( activateContractWallet,
      callEndpoint,
      waitNSlots,
      runEmulatorTraceIO )
import PlutusTx (Data (..))
import qualified PlutusTx
import PlutusTx.Prelude
    ( Bool (True, False),
      Integer,
      (.),
      (&&),
      (<$>),
      mconcat,
      ($),
      fst,
      traceIfFalse,
      Ord((<)) )
import Text.Printf (printf)
import Wallet.Emulator.Wallet ( Wallet(Wallet), walletPubKey )
import Prelude (IO, Semigroup (..), Show (..), String)

data VestingParam = VestingParam
  { beneficiary :: PubKeyHash,
    deadline :: POSIXTime
  }
  deriving (Show)

PlutusTx.makeLift ''VestingParam

{-# INLINEABLE mkValidator #-}
mkValidator :: () -> () -> ScriptContext -> Bool
mkValidator () () _ = True 

data Vesting

instance Scripts.ValidatorTypes Vesting where
  type DatumType Vesting = ()
  type RedeemerType Vesting = ()

typedValidator :: Scripts.TypedValidator Vesting
typedValidator =
  Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [||mkValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @() @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

type VestingSchema =
  Endpoint "give" ()
    .\/ Endpoint "grab" ()

give :: AsContractError e => Contract w s e ()
give = do
  let tx = mustPayToTheScript () $ Ada.lovelaceValueOf 1_000_000
  ledgerTx <- submitTxConstraints typedValidator tx
  void $ awaitTxConfirmed $ txId ledgerTx
  logInfo @String "Everything went well"

endpoints :: Contract () VestingSchema Text ()
endpoints = give' >> endpoints
  where
    give' = endpoint @"give" >> give

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []

testMarket :: IO ()
testMarket = runEmulatorTraceIO $ do
  h1 <- activateContractWallet (Wallet 1) endpoints
  callEndpoint @"give" h1 ()
  void $ waitNSlots 1
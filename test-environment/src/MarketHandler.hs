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
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module MarketHandler where

import Control.Monad (Monad ((>>), (>>=)), void)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map as Map (filter, null, toList)
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger
  ( Address,
    CurrencySymbol,
    Datum,
    DatumHash,
    POSIXTime,
    PubKeyHash,
    Redeemer (Redeemer),
    ScriptContext (scriptContextTxInfo),
    TxInfo (txInfoValidRange),
    TxOut,
    TxOutRef,
    TxOutTx,
    Validator,
    ValidatorHash,
    Value,
    contains,
    from,
    pubKeyHash,
    scriptAddress,
    txData,
    txId,
    txOutDatum,
    txOutputs,
    txSignedBy,
  )
import Ledger.Ada as Ada (lovelaceValueOf)
import Ledger.Constraints as Constraints
  ( TxConstraints,
    mustPayToTheScript,
    mustSpendScriptOutput,
    mustValidateIn,
    otherScript,
    unspentOutputs,
  )
import Ledger.Scripts (Datum (Datum), DatumHash)
import Ledger.Typed.Scripts (Validator)
import Ledger.Value (assetClassValue)
import qualified Ledger.Typed.Scripts as Scripts
import Marketplace (Marketplace, defMarketplace)
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
    runEmulatorTraceIO,
    waitNSlots,
  )
import PlutusTx (Data (..))
import qualified PlutusTx
import PlutusTx.AssocMap
  ( Map,
    delete,
    fromList,
    insert,
    keys,
  )
import PlutusTx.Prelude
  ( Bool (False, True),
    Integer,
    Maybe (Just, Nothing),
    Ord ((<)),
    fst,
    map,
    mconcat,
    traceIfFalse,
    ($),
    (&&),
    (.),
    (<$>),
  )
import Service (Service, defService)
import Text.Printf (printf)
import Wallet.Emulator.Wallet (Wallet (Wallet), walletPubKey)
import Prelude (Eq, IO, Ord, Semigroup (..), Show (..), String, lookup)
import Helper (valHash)

{-# INLINEABLE marketDatum #-}
marketDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Marketplace
marketDatum o f = do
  dh <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromBuiltinData d

data MarketRedeemer = Offer | Request | Distribute | Collect
  deriving (Show)

PlutusTx.unstableMakeIsData ''MarketRedeemer

data ServiceRequest
  = None
  | ServiceRequest
      { accusationValidatorHash :: !ValidatorHash,
        service :: !Service
      }
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''ServiceRequest

{-# INLINEABLE mkMarketplaceValidator #-}
mkMarketplaceValidator :: ServiceRequest -> Marketplace -> MarketRedeemer -> ScriptContext -> Bool
mkMarketplaceValidator _ _ _ _ = True

data MarketHandlerType

instance Scripts.ValidatorTypes MarketHandlerType where
  type DatumType MarketHandlerType = Marketplace
  type RedeemerType MarketHandlerType = MarketRedeemer

typedValidator :: ServiceRequest -> Scripts.TypedValidator MarketHandlerType
typedValidator sr =
  Scripts.mkTypedValidator @MarketHandlerType
    ($$(PlutusTx.compile [||mkMarketplaceValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode sr)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @Marketplace @MarketRedeemer

marketValidator :: ServiceRequest -> Validator
marketValidator = Scripts.validatorScript . typedValidator

marketValidatorHash :: ServiceRequest -> Ledger.ValidatorHash
marketValidatorHash = Scripts.validatorHash . typedValidator

marketAddress :: ServiceRequest -> Ledger.Address
marketAddress = scriptAddress . marketValidator

type MarketSchema =
  Endpoint "offer" ServiceRequest
    .\/ Endpoint "request" ()

data MarketContext = MarketContext
  { mSymbol :: !CurrencySymbol,
    mMinimumValue :: !Value,
    mPlatformFees :: !Integer
  }
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''MarketContext

-- startOracle :: forall w s. OracleParams -> Contract w s Text Oracle
-- startOracle op = do
--     pkh <- pubKeyHash <$> Contract.ownPubKey
--     osc <- mapError (pack . show) (mintContract pkh [(oracleTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
--     let cs     = Currency.currencySymbol osc
--         oracle = Oracle
--             { oSymbol   = cs
--             , oOperator = pkh
--             , oFee      = opFees op
--             , oAsset    = AssetClass (opSymbol op, opToken op)
--             }
--     logInfo @String $ "started oracle " ++ show oracle
--     return oracle

-- startMarketplace :: forall w s. MarketContext -> Contract w s Text ()
-- startMarketplace mktCtx = do
--   pkh <- pubKeyHash <$> Contract.ownPubKey
--   mkc <- mapError (pack . show) (mintContract pkh [])

offer :: AsContractError e => ServiceRequest -> Contract w s e ()
offer sr = do
  let tx =
        mustPayToTheScript defMarketplace (Ada.lovelaceValueOf 1_000_000)
  ledgerTx <- submitTxConstraints (typedValidator sr) tx
  void $ awaitTxConfirmed $ txId ledgerTx
  logInfo @String $ show $ map (\o -> marketDatum o (\dh -> lookup dh (toList (txData ledgerTx)))) $ txOutputs ledgerTx
  logInfo @String "Everything went well"

endpoints :: Contract () MarketSchema Text ()
endpoints = offer' >> endpoints
  where
    offer' = endpoint @"offer" >>= offer

mkKnownCurrencies []

testMarket :: IO ()
testMarket = runEmulatorTraceIO $ do
  h1 <- activateContractWallet (Wallet 1) endpoints
  let sr = ServiceRequest (valHash 2) defService
  callEndpoint @"offer" h1 sr
  void $ waitNSlots 1
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

module Account where

import Control.Monad (void)
import Data.Aeson hiding (Value)
import GHC.Generics
import Ledger
import Ledger.Ada
import Ledger.Scripts
import Ledger.Typed.Scripts as Scripts hiding (validatorHash)
import Ledger.Value
import Playground.Contract
import Plutus.ChainIndex
import qualified PlutusTx
import qualified PlutusTx.AssocMap as PlutusMap
import PlutusTx.Prelude
import qualified PlutusTx.Ratio as R
import Utils
import qualified Prelude

data AccountSettings = AccountSettings
  { casAccValHash :: ValidatorHash,
    casToken :: AssetClass,
    casEntranceFee :: Integer,
    casTickets :: [AssetClass]
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq)

instance Eq AccountSettings where
  {-# INLINEABLE (==) #-}
  (AccountSettings vh tn entFee tkt)
    == (AccountSettings vh' tn' entFee' tkt') =
      vh == vh' && tn == tn' && entFee == entFee' && tkt == tkt'

PlutusTx.unstableMakeIsData ''AccountSettings
PlutusTx.makeLift ''AccountSettings

-- The datatype that represents the account information on-chain
data AccountDatum = AccountDatum
  { adCAS :: Integer,
    adContracts :: [AssetClass],
    adSigSymbol :: CurrencySymbol,
    adTickets :: [AssetClass]
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq AccountDatum where
  {-# INLINEABLE (==) #-}
  (AccountDatum cas ctrs sig tkts) == (AccountDatum cas' ctrs' sig' tkts') =
    cas == cas' && ctrs == ctrs' && sig == sig' && tkts == tkts'

PlutusTx.unstableMakeIsData ''AccountDatum

{-# INLINEABLE findAccountDatum #-}
findAccountDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe AccountDatum
findAccountDatum o f = do
  dh <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromBuiltinData d

-- The expected initial datum
{-# INLINEABLE initDatum #-}
initDatum ::
  CurrencySymbol ->
  [AssetClass] ->
  AccountDatum
initDatum cs tkts =
  AccountDatum
    { adCAS = 60_000,
      adContracts = [],
      adSigSymbol = cs,
      adTickets = tkts
    }

data AccountInfo = AccountInfo
  { aiSig :: Integer,
    aiFees :: Integer,
    aiCAS :: Integer,
    aiContracts :: [AssetClass],
    aiSigSymbol :: CurrencySymbol,
    aiTickets :: [AssetClass]
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq AccountInfo where
  {-# INLINEABLE (==) #-}
  (AccountInfo s f cas ctr ss t) == (AccountInfo s' f' cas' ctr' ss' t') =
    s == s' && f == f' && cas == cas' && ctr == ctr' && ss == ss' && t == t'

PlutusTx.unstableMakeIsData ''AccountInfo

parseAccount ::
  PubKeyHash ->
  ChainIndexTxOut ->
  AccountDatum ->
  AccountInfo
parseAccount pkh out dat =
  AccountInfo
    { aiSig = sig,
      aiFees = fees,
      aiCAS = adCAS dat,
      aiContracts = adContracts dat,
      aiSigSymbol = adSigSymbol dat,
      aiTickets = adTickets dat
    }
  where
    val :: Value
    val = txOutValue (toTxOut out)

    sig :: Integer
    sig = valueOf val (adSigSymbol dat) (parsePubKeyHash pkh)

    fees :: Integer
    fees = (getLovelace . fromValue) val

{-# INLINEABLE addContractToAccount #-}
addContractToAccount :: AccountDatum -> AssetClass -> AccountDatum
addContractToAccount dat cnt =
  AccountDatum
    { adCAS = adCAS dat,
      adContracts = cnt : (adContracts dat),
      adSigSymbol = adSigSymbol dat,
      adTickets = adTickets dat
    }

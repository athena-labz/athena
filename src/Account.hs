{-# LANGUAGE BangPatterns #-}
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
import Ledger
import Ledger.Ada
import Ledger.Scripts
import Ledger.Typed.Scripts as Scripts hiding (validatorHash)
import Ledger.Value
import Playground.Contract (Generic, ToSchema)
import Plutus.ChainIndex
import qualified PlutusTx
import qualified PlutusTx.AssocMap as PlutusMap
import PlutusTx.Prelude (Eq, Maybe (..), Integer, (==), (&&), (.))
import qualified PlutusTx.Ratio as R
import Utils
import qualified Prelude

data AccountSettings = AccountSettings
  { casAccValHash :: !ValidatorHash,
    casToken :: !AssetClass,
    casEntranceFee :: !Integer,
    casTickets :: ![CurrencySymbol]
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq)

instance Eq AccountSettings where
  {-# INLINEABLE (==) #-}
  (AccountSettings vh tn entFee tkt)
    == (AccountSettings vh' tn' entFee' tkt') =
      vh == vh' && tn == tn' && entFee == entFee' && tkt == tkt'

-- The datatype that represents the account information on-chain
data AccountDatum = AccountDatum
  { adCAS :: !Integer,
    adContracts :: ![AssetClass],
    adSigSymbol :: !CurrencySymbol,
    adTickets :: ![CurrencySymbol]
  }
  deriving (Prelude.Show, Prelude.Eq)

instance Eq AccountDatum where
  {-# INLINEABLE (==) #-}
  (AccountDatum cas ctrs sig tkts) == (AccountDatum cas' ctrs' sig' tkts') =
    cas == cas' && ctrs == ctrs' && sig == sig' && tkts == tkts'

{-# INLINEABLE findAccountDatum #-}
findAccountDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe AccountDatum
findAccountDatum o f = do
  dh <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromBuiltinData d

data AccountInfo = AccountInfo
  { aiSig :: !Integer,
    aiFees :: !Integer,
    aiCAS :: !Integer,
    aiContracts :: ![AssetClass],
    aiSigSymbol :: !CurrencySymbol,
    aiTickets :: ![CurrencySymbol]
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq AccountInfo where
  {-# INLINEABLE (==) #-}
  (AccountInfo s f cas ctr ss t) == (AccountInfo s' f' cas' ctr' ss' t') =
    s == s' && f == f' && cas == cas' && ctr == ctr' && ss == ss' && t == t'

-- The expected initial datum
{-# INLINEABLE initDatum #-}
initDatum ::
  CurrencySymbol ->
  [CurrencySymbol] ->
  AccountDatum
initDatum cs tkts =
  AccountDatum
    { adCAS = 60_000,
      adContracts = [],
      adSigSymbol = cs,
      adTickets = tkts
    }

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

{-# INLINEABLE changeAccountCAS #-}
changeAccountCAS :: AccountDatum -> Integer -> AccountDatum
changeAccountCAS dat cas =
  AccountDatum
    { adCAS = cas,
      adContracts = adContracts dat,
      adSigSymbol = adSigSymbol dat,
      adTickets = adTickets dat
    }

PlutusTx.unstableMakeIsData ''AccountSettings
PlutusTx.unstableMakeIsData ''AccountDatum
PlutusTx.unstableMakeIsData ''AccountInfo

PlutusTx.makeLift ''AccountSettings

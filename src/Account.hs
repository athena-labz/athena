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

-- The datatype that represents the account information on-chain
data AccountDatum = AccountDatum
  { adCAS :: Integer,
    adContracts :: [AssetClass],
    adSigSymbol :: CurrencySymbol,
    adTickets :: PlutusMap.Map TokenName CurrencySymbol
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
  PlutusMap.Map TokenName CurrencySymbol ->
  AccountDatum
initDatum cs tkts =
  AccountDatum
    { adCAS = 60_000,
      adContracts = [],
      adSigSymbol = cs,
      adTickets = tkts
    }
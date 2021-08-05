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

module Service where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger (PubKeyHash, Value, pubKeyHash)
import Ledger.Value as Value (geq, singleton)
import qualified PlutusTx
import PlutusTx.Prelude (Bool (False, True), ByteString, Eq, ($), (&&), (==))
import Wallet.Emulator.Wallet (Wallet (Wallet), walletPubKey)
import Prelude (Semigroup (..), Show (..))
import qualified Prelude

data ServiceState = Available | Sold
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq ServiceState where
  Available == Available = True
  Sold == Sold = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''ServiceState

data Service = Service
  { publisher :: PubKeyHash, -- The pkh of the person that published this service
    title :: ByteString,
    description :: ByteString,
    price :: Value,
    trust :: Value,
    state :: ServiceState -- Indicates the service availability
  }
  deriving (Show, Generic, FromJSON, ToJSON)

instance Eq Service where
  (Service pu ti d pr tr s) == (Service pu' ti' d' pr' tr' s') =
    pu == pu' && ti == ti' && d == d' && pr == pr' && tr == tr' && s == s'

PlutusTx.unstableMakeIsData ''Service

-- Defaults for debugging purposes
{-# INLINEABLE defPrice #-}
defPrice :: Value
defPrice = singleton "ff" "DSET" 300

{-# INLINEABLE defTrust #-}
defTrust :: Value
defTrust = singleton "ff" "DSET" 30

{-# INLINEABLE defService #-}
defService :: Service
defService =
  Service
    { publisher = pubKeyHash $ walletPubKey $ Wallet 1,
      title = "Title",
      description = "Description",
      price = defPrice,
      trust = defTrust,
      state = Available
    }

{-# INLINEABLE paidOffer #-}
paidOffer :: Value -> Value -> Service -> Bool
paidOffer input output s = output `geq` (input <> trust s)

{-# INLINEABLE paidRequest #-}
paidRequest :: Value -> Value -> Service -> Bool
paidRequest input output s = output `geq` (input <> trust s <> price s)

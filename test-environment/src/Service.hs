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
import PlutusTx.Prelude (Bool, ByteString, ($))
import Wallet.Emulator.Wallet (Wallet (Wallet), walletPubKey)
import Prelude (Semigroup (..), Show (..))
import qualified Prelude

data ServiceState = Available | Sold
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.unstableMakeIsData ''ServiceState

data Service = Service
  { publisher :: PubKeyHash, -- The pkh of the person that published this service
    title :: ByteString,
    description :: ByteString,
    price :: Value,
    trust :: Value,
    state :: ServiceState -- Indicates the service availability
  }
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.unstableMakeIsData ''Service

-- Defaults for debugging purposes
defPrice :: Value
defPrice = singleton "ff" "DSET" 300

defTrust :: Value
defTrust = singleton "ff" "DSET" 30

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

{-# INLINABLE paidOffer #-}
paidOffer :: Value -> Value -> Service -> Bool
paidOffer input output s = output `geq` (input <> trust s)

{-# INLINABLE paidRequest #-}
paidRequest :: Value -> Value -> Service -> Bool
paidRequest input output s = output `geq` (input <> trust s <> price s)

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

module Membership.PlatformSettings where

import Control.Monad ()
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Ada as Ada ()
import Ledger.Constraints as Constraints ()
import Ledger.Crypto (PubKeyHash)
import Ledger.Scripts (ValidatorHash)
import Ledger.Value (AssetClass, CurrencySymbol)
import Plutus.Contract as Contract ()
import Plutus.Contract.StateMachine ()
import qualified PlutusTx
import PlutusTx.Prelude (Eq, Integer, (&&), (==))
import Prelude (Show (..))
import qualified Prelude

-- Separate universal aspects

data PlatformSettings = PlatformSettings
  { psVersion :: !Integer,
    psToken :: !AssetClass,
    psEntranceFee :: !Integer,
    psTxFee :: !Integer
  }
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''PlatformSettings

data AccountSettings = AccountSettings
  { asPlatformSettings :: !PlatformSettings,
    asSignatureSymbol :: !CurrencySymbol,
    asContractValidatorHash :: !ValidatorHash,
    asCollectors :: ![PubKeyHash]
  }
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''AccountSettings

-- Change platform token to platform settings
data ContractSettings = ContractSettings
  { csPlatformToken :: AssetClass,
    csSignatureSymbol :: CurrencySymbol
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq ContractSettings where
  {-# INLINEABLE (==) #-}
  ContractSettings pt ss == ContractSettings pt' ss' =
    pt == pt' && ss == ss'

PlutusTx.makeLift ''ContractSettings

{-# INLINEABLE contractSettings #-}
contractSettings :: AssetClass -> CurrencySymbol -> ContractSettings
contractSettings ac cs =
  ContractSettings
    { csPlatformToken = ac,
      csSignatureSymbol = cs
    }

-- data PlatformSettings = PlatformSettings
--   { psVersion :: !Integer,
--     psToken :: !AssetClass,
--     psSignatureSymbol :: !CurrencySymbol,
--     psContractVH :: !ValidatorHash,
--     psEntranceFee :: !Integer,
--     psTxFee :: !Integer,
--     psCollectors :: ![PubKeyHash],
--     psOldVH :: !(Maybe ValidatorHash)
--   }
--   deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)
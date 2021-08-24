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

module Membership.PlatformSettings where

import Control.Monad ()
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Ada as Ada ()
import Ledger.Constraints as Constraints ()
import Ledger.Crypto ( PubKeyHash )
import Ledger.Scripts (ValidatorHash)
import Ledger.Value ( CurrencySymbol, AssetClass )
import Plutus.Contract as Contract ()
import Plutus.Contract.StateMachine ()
import qualified PlutusTx
import PlutusTx.Prelude ( Integer, Maybe )
import Prelude (Show (..))
import qualified Prelude

data PlatformSettings = PlatformSettings
  { psVersion :: !Integer,
    psToken :: !AssetClass,
    psSignatureSymbol :: !CurrencySymbol,
    psContractVH :: !ValidatorHash,
    psEntranceFee :: !Integer,
    psTxFee :: !Integer, -- Should be a percentage (0 - 100)
    psCollectors :: ![PubKeyHash],
    psOldVH :: !(Maybe ValidatorHash)
  }
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''PlatformSettings

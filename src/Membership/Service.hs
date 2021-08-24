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

module Membership.Service where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger (POSIXTime, PubKeyHash, Value)
import Ledger.Value as Value (geq, singleton)
import qualified PlutusTx
import PlutusTx.Prelude (Bool (False, True), ByteString, Eq, Integer, (&&), (==))
import Prelude (Semigroup (..), Show (..))
import qualified Prelude

data ServiceType = CConstant | OneTime Value POSIXTime
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq ServiceType where
  {-# INLINEABLE (==) #-}
  CConstant == CConstant = True
  OneTime p d == OneTime p' d' = p == p' && d == d'
  _ == _ = False

PlutusTx.unstableMakeIsData ''ServiceType

data Service = Service
  { sPublisher :: PubKeyHash, -- The pkh of the person that published this service
    sTitle :: ByteString,
    sDescription :: ByteString,
    sTrust :: Integer,
    sType :: ServiceType -- Indicates the service availability
  }
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Service where
  {-# INLINEABLE (==) #-}
  (Service pu ti d tr tp) == (Service pu' ti' d' tr' tp') =
    pu == pu' && ti == ti' && d == d' && tr == tr' && tp == tp'

PlutusTx.unstableMakeIsData ''Service

{-# INLINEABLE paidOffer #-}
paidOffer :: Value -> Value -> Service -> Bool
paidOffer input output s = output `geq` (input <> singleton "ff" "DSET" (sTrust s))

{-# INLINEABLE paidRequest #-}
paidRequest :: Value -> Value -> Service -> Bool
paidRequest input output s = case s of
  (Service _ _ _ t (OneTime p _)) -> output `geq` (input <> singleton "ff" "DSET" t <> p)
  (Service _ _ _ t CConstant) -> output `geq` (input <> singleton "ff" "DSET" t)
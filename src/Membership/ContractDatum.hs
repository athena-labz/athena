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

module Membership.ContractDatum where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Contexts (TxOut (TxOut))
import Ledger.Crypto (PubKeyHash)
import Ledger.Scripts (ValidatorHash)
import Ledger.Time (POSIXTime)
import Ledger.Value (Value)
import Membership.Service (Service)
import qualified PlutusTx
import PlutusTx.Prelude (Bool, ByteString, Eq (..), length, (&&), (>))
import qualified Prelude

type Accusation = (PubKeyHash, PubKeyHash)

data ContractDatum = ContractDatum
  { cdJudges :: [PubKeyHash],
    cdInputs :: [ByteString],
    cdLogicScript :: ValidatorHash,
    cdAccusations :: [(Accusation, POSIXTime)],
    cdService :: Service
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON)

instance Eq ContractDatum where
  {-# INLINEABLE (==) #-}
  (ContractDatum jud inp ls acc svc) == (ContractDatum jud' inp' ls' acc' svc') =
    jud == jud' && inp == inp' && ls == ls' && acc == acc' && svc == svc'

PlutusTx.unstableMakeIsData ''ContractDatum

type DigiContract = (ContractDatum, Value)

{-# INLINEABLE digiContract #-}
digiContract :: ContractDatum -> TxOut -> DigiContract
digiContract cd (TxOut _ v _) = (cd, v)

{-# INLINEABLE isInitial #-}
isInitial :: ContractDatum -> Bool
isInitial (ContractDatum jud inp _ acc _) =
  length jud > 5
    && length inp > 0
    && acc == []
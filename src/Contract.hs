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

module Contract where

import Control.Monad (void)
import Data.Aeson hiding (Value)
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

data ContractSettings = ContractSettings
  { ccsAccValHash :: ValidatorHash,
    ccsCtrValHash :: ValidatorHash,
    ccsToken :: AssetClass
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq ContractSettings where
  {-# INLINEABLE (==) #-}
  (ContractSettings avh cvh t)
    == (ContractSettings avh' cvh' t') =
      avh == avh' && cvh == cvh' && t == t'

PlutusTx.unstableMakeIsData ''ContractSettings
PlutusTx.makeLift ''ContractSettings

data RelationType = RT_Convergent | RT_Distributed
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq RelationType where
  {-# INLINEABLE (==) #-}
  RT_Convergent == RT_Convergent = True
  RT_Distributed == RT_Distributed = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''RelationType
PlutusTx.makeLift ''RelationType

data PrivacyType = PT_Public | PT_Private
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq PrivacyType where
  {-# INLINEABLE (==) #-}
  PT_Public == PT_Public = True
  PT_Private == PT_Private = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''PrivacyType
PlutusTx.makeLift ''PrivacyType

data BoolInput = BoolInput
  { biRoles :: [Integer],
    biQuestion :: BuiltinByteString
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq BoolInput where
  {-# INLINEABLE (==) #-}
  (BoolInput r q) == (BoolInput r' q') = r == r' && q == q'

PlutusTx.unstableMakeIsData ''BoolInput
PlutusTx.makeLift ''BoolInput

data IntegerInput = IntegerInput
  { iiRoles :: [Integer],
    iiQuestion :: BuiltinByteString
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq IntegerInput where
  {-# INLINEABLE (==) #-}
  (IntegerInput r q) == (IntegerInput r' q') = r == r' && q == q'

PlutusTx.unstableMakeIsData ''IntegerInput
PlutusTx.makeLift ''IntegerInput

-- Triggers are not cummulative, so the worst punishments must come first and
-- all cases should be accounted for
data Trigger
  = TrigEqualsInt IntegerInput Integer
  | TrigEqualsIntInp IntegerInput IntegerInput
  | TrigEqualsBool BoolInput Bool
  | TrigEqualsBoolInp BoolInput BoolInput
  | TrigGreaterThan IntegerInput Integer
  | TrigGreaterThanInp IntegerInput IntegerInput
  | TrigLessThan IntegerInput Integer
  | TrigLessThanInp IntegerInput IntegerInput
  | TrigNot Trigger
  | TrigAnd Trigger Trigger
  | TrigOr Trigger Trigger
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Trigger where
  {-# INLINEABLE (==) #-}
  (TrigEqualsInt ii i) == (TrigEqualsInt ii' i') = ii == ii' && i == i'
  (TrigEqualsBool bi b) == (TrigEqualsBool bi' b') = bi == bi' && b == b'
  (TrigGreaterThan ii i) == (TrigGreaterThan ii' i') = ii == ii' && i == i'
  (TrigLessThan ii i) == (TrigLessThan ii' i') = ii == ii' && i == i'
  (TrigNot t) == (TrigNot t') = t == t'
  (TrigAnd t1 t2) == (TrigAnd t1' t2') = t1 == t1' && t2 == t2'
  (TrigOr t1 t2) == (TrigOr t1' t2') = t1 == t1' && t2 == t2'
  _ == _ = False

PlutusTx.unstableMakeIsData ''Trigger
PlutusTx.makeLift ''Trigger

data Action
  = DecreaseCAS R.Rational
  | SuspendFromPlatform POSIXTime
  | ExpelFromContract
  | PayFromCollateral Value
  | DoInRealLife BuiltinByteString
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Action where
  {-# INLINEABLE (==) #-}
  (DecreaseCAS p) == (DecreaseCAS p') = p == p'
  (SuspendFromPlatform t) == (SuspendFromPlatform t') = t == t'
  ExpelFromContract == ExpelFromContract = True
  (PayFromCollateral v) == (PayFromCollateral v') = v == v'
  (DoInRealLife bs) == (DoInRealLife bs') = bs == bs'
  _ == _ = False

PlutusTx.unstableMakeIsData ''Action
PlutusTx.makeLift ''Action

data Accusation = Accusation
  { aAccuser :: PubKeyHash,
    aAccused :: PubKeyHash,
    aTime :: POSIXTime
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Accusation where
  {-# INLINEABLE (==) #-}
  (Accusation acr acd t) == (Accusation acr' acd' t') =
    acr == acr' && acd == acd' && t == t'

PlutusTx.unstableMakeIsData ''Accusation
PlutusTx.makeLift ''Accusation

data ContractDatum = ContractDatum
  { cdRelationType :: RelationType,
    cdPrivacyType :: PrivacyType,
    cdPublisher :: PubKeyHash,
    cdCollateral :: Value, -- Must be positive
    cdTerms :: PlutusMap.Map Trigger [Action],
    cdJudges :: [Address],
    cdAccusations :: [Accusation],
    cdRoles :: Integer, -- The maximum role index
    cdRoleMap :: PlutusMap.Map PubKeyHash Integer,
    cdTickets :: [AssetClass]
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq ContractDatum where
  {-# INLINEABLE (==) #-}
  ContractDatum rt pt p c t j a r rm tk
    == ContractDatum rt' pt' p' c' t' j' a' r' rm' tk' =
      rt == rt'
        && pt == pt'
        && p == p'
        && c == c'
        && t == t'
        && j == j'
        && a == a'
        && r == r'
        && rm == rm'
        && tk == tk'

PlutusTx.unstableMakeIsData ''ContractDatum
PlutusTx.makeLift ''ContractDatum

{-# INLINEABLE findContractDatum #-}
findContractDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe ContractDatum
findContractDatum o f = do
  dh <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromBuiltinData d

{-# INLINABLE validRoles #-}
validRoles :: ContractDatum -> Bool
validRoles dat = all p (PlutusMap.elems $ cdRoleMap dat)
  where
    maxRole :: Integer
    maxRole = cdRoles dat

    p :: Integer -> Bool
    p n = n <= maxRole
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
import Utils

data ContractSettings = ContractSettings
  { ccsAccValHash :: !ValidatorHash,
    ccsCtrValHash :: !ValidatorHash,
    ccsToken :: !AssetClass
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
  RT_Convergent == RT_Distributed = False
  _ == _ = True

PlutusTx.unstableMakeIsData ''RelationType
PlutusTx.makeLift ''RelationType

data PrivacyType = PT_Public | PT_Private
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq PrivacyType where
  {-# INLINEABLE (==) #-}
  PT_Public == PT_Private = False
  _ == _ = True

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
  | SuspendFromContract POSIXTime
  | PayFromCollateral Value
  | DoInRealLife BuiltinByteString
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Action where
  {-# INLINEABLE (==) #-}
  (DecreaseCAS p) == (DecreaseCAS p') = p == p'
  (SuspendFromContract t) == (SuspendFromContract t') = t == t'
  (PayFromCollateral v) == (PayFromCollateral v') = v == v'
  (DoInRealLife bs) == (DoInRealLife bs') = bs == bs'
  _ == _ = False

PlutusTx.unstableMakeIsData ''Action
PlutusTx.makeLift ''Action

data Accusation = Accusation
  { aAccuser :: !PubKeyHash,
    aAccused :: !PubKeyHash,
    aTime :: !POSIXTime,
    aDeadline :: !POSIXTime
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Accusation where
  {-# INLINEABLE (==) #-}
  (Accusation acr acd t dln) == (Accusation acr' acd' t' dln') =
    acr == acr' && acd == acd' && t == t' && dln == dln'

PlutusTx.unstableMakeIsData ''Accusation
PlutusTx.makeLift ''Accusation

data ContractDatum = ContractDatum
  { cdSigSymbol :: !CurrencySymbol,
    cdRelationType :: !RelationType,
    cdPrivacyType :: !PrivacyType,
    cdPublisher :: !PubKeyHash,
    cdCollateral :: !Value, -- Must be positive
    cdTermsHash :: !BuiltinByteString,
    cdJudges :: ![Address],
    cdAccusations :: ![Accusation],
    cdResolutions :: ![(Accusation, BuiltinByteString)],
    cdRoles :: !Integer, -- The maximum role index
    cdRoleMap :: !(PlutusMap.Map PubKeyHash Integer),
    cdTickets :: ![CurrencySymbol]
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq ContractDatum where
  {-# INLINEABLE (==) #-}
  ContractDatum s rt pt p c t j a rs r rm tk
    == ContractDatum s' rt' pt' p' c' t' j' a' rs' r' rm' tk' =
      s == s'
        && rt == rt'
        && pt == pt'
        && p == p'
        && c == c'
        && t == t'
        && j == j'
        && a == a'
        && rs == rs'
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

{-# INLINEABLE addUserToContract #-}
addUserToContract :: PubKeyHash -> Integer -> ContractDatum -> ContractDatum
addUserToContract pkh role dat =
  ContractDatum
    { cdSigSymbol = cdSigSymbol dat,
      cdRelationType = cdRelationType dat,
      cdPrivacyType = cdPrivacyType dat,
      cdPublisher = cdPublisher dat,
      cdCollateral = cdCollateral dat,
      cdTermsHash = cdTermsHash dat,
      cdJudges = cdJudges dat,
      cdAccusations = cdAccusations dat,
      cdResolutions = cdResolutions dat,
      cdRoles = cdRoles dat,
      cdRoleMap = PlutusMap.insert pkh role (cdRoleMap dat),
      cdTickets = cdTickets dat
    }

{-# INLINEABLE addAccusationToContract #-}
addAccusationToContract :: Accusation -> ContractDatum -> ContractDatum
addAccusationToContract acc dat =
  ContractDatum
    { cdSigSymbol = cdSigSymbol dat,
      cdRelationType = cdRelationType dat,
      cdPrivacyType = cdPrivacyType dat,
      cdPublisher = cdPublisher dat,
      cdCollateral = cdCollateral dat,
      cdTermsHash = cdTermsHash dat,
      cdJudges = cdJudges dat,
      cdAccusations = acc : cdAccusations dat,
      cdResolutions = cdResolutions dat,
      cdRoles = cdRoles dat,
      cdRoleMap = cdRoleMap dat,
      cdTickets = cdTickets dat
    }

-- ! We are assuming the judge will always judge the last case
{-# INLINEABLE resolveDisputeInContract #-}
resolveDisputeInContract :: BuiltinByteString -> ContractDatum -> ContractDatum
resolveDisputeInContract vdt dat =
  ContractDatum
    { cdSigSymbol = cdSigSymbol dat,
      cdRelationType = cdRelationType dat,
      cdPrivacyType = cdPrivacyType dat,
      cdPublisher = cdPublisher dat,
      cdCollateral = cdCollateral dat,
      cdTermsHash = cdTermsHash dat,
      cdJudges = cdJudges dat,
      cdAccusations = init (cdAccusations dat),
      cdResolutions = ((acc, vdt) : cdResolutions dat),
      cdRoles = cdRoles dat,
      cdRoleMap = cdRoleMap dat,
      cdTickets = cdTickets dat
    }
  where
    acc :: Accusation
    acc = last (cdAccusations dat)

{-# INLINEABLE validRoles #-}
validRoles :: ContractDatum -> Bool
validRoles dat = all p (PlutusMap.elems $ cdRoleMap dat)
  where
    maxRole :: Integer
    maxRole = cdRoles dat

    p :: Integer -> Bool
    p n = n <= maxRole

-- TODO: Randomly select judge
{-# INLINEABLE currentJudge #-}
currentJudge :: ContractDatum -> Maybe Address
currentJudge dat = case cdJudges dat of
  [] -> Nothing
  (x : xs) -> Just x

-- What our off-chain code will actually need to create a contract.
-- Other parameters can be derived
data ContractCore = ContractCore
  { ccRelationType :: RelationType,
    ccPrivacyType :: PrivacyType,
    ccCollateral :: Value,
    ccTermsHash :: BuiltinByteString,
    ccJudges :: [Address],
    ccRoles :: Integer,
    ccRoleMap :: PlutusMap.Map PubKeyHash Integer,
    ccTickets :: [CurrencySymbol]
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq ContractCore where
  {-# INLINEABLE (==) #-}
  ContractCore rt pt c t j r rm tk
    == ContractCore rt' pt' c' t' j' r' rm' tk' =
      rt == rt'
        && pt == pt'
        && c == c'
        && t == t'
        && j == j'
        && r == r'
        && rm == rm'
        && tk == tk'

PlutusTx.unstableMakeIsData ''ContractCore
PlutusTx.makeLift ''ContractCore
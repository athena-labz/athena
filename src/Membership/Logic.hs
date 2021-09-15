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

module Membership.Logic where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger
  ( Datum (Datum),
    DatumHash,
    PubKeyHash,
    TxOut,
    ValidatorHash,
    txOutDatum,
  )
import Ledger.Value (AssetClass, CurrencySymbol)
import Membership.Contract (Accusation, Judge)
import Membership.PlatformSettings (PlatformSettings)
import Membership.Signature (Sig, sigIn)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Prelude
  ( AdditiveSemigroup ((+)),
    Bool (..),
    BuiltinByteString,
    Eq (..),
    Integer,
    Maybe (..),
    MultiplicativeSemigroup ((*)),
    all,
    any,
    elem,
    find,
    length,
    return,
    traceError,
    ($),
    (&&),
    (/=),
  )
import qualified PlutusTx.Ratio as R
import qualified Prelude

-- Conditions are input requirements. For example,
--    CAny [CCondition "Was the book well received?",
--          CCondition "Did the book have more than 200 pages?"]
--    would mean that if any of these conditions are not met
--    (the verdict doesn't correspond to the expected input),
--    the accused would be declared guilty

-- There should only be conditions included in the "inputs" map
data Conditions
  = CAny [Conditions]
  | CAll [Conditions]
  | CCondition BuiltinByteString
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Conditions where
  {-# INLINEABLE (==) #-}
  CCondition bs == CCondition bs' = bs == bs'
  CAll cds == CAll cds' = all (`elem` cds') cds && length cds == length cds'
  CAny cds == CAny cds' = all (`elem` cds') cds && length cds == length cds'
  CAny [cd] == CAll [cd'] = cd == cd'
  _ == _ = False

PlutusTx.unstableMakeIsData ''Conditions
PlutusTx.makeLift ''Conditions

type AccuserAmt = Integer

type AccusedAmt = Integer

-- 1 : 1 would mean only half would be taken from the accused's trust collateral amount
data Proportion = Proportion AccuserAmt AccusedAmt
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.unstableMakeIsData ''Proportion
PlutusTx.makeLift ''Proportion

-- Conditions and the poportion, by which the trust token will be
-- distributed, if these conditions are not met
type Logic = Map.Map Conditions Proportion

-- Should contain all inputs
type Verdict = Map.Map BuiltinByteString Bool

-- Mapping question and expected answer
type Inputs = Map.Map BuiltinByteString Bool

data LogicSettings = LogicSettings
  { lsPlatformSettings :: PlatformSettings,
    lsSignatureSymbol :: CurrencySymbol,
    lsContract :: (ValidatorHash, AssetClass),
    lsInputs :: Inputs,
    lsLogic :: Logic
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''LogicSettings

type Accuser = PubKeyHash

type Accused = PubKeyHash

data LogicState = LSWaitingStart | LSWaitingVerdict Judge Accusation | LSWaitingEnd Judge Accusation Verdict
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq LogicState where
  {-# INLINEABLE (==) #-}
  LSWaitingStart == LSWaitingStart = True
  (LSWaitingVerdict j acc) == (LSWaitingVerdict j' acc') =
    j == j' && acc == acc'
  (LSWaitingEnd j a v) == (LSWaitingEnd j' a' v') =
    j == j' && a == a' && v == v'
  _ == _ = False

PlutusTx.unstableMakeIsData ''LogicState

data LogicRedeemer
  = LRAccuse Accusation
  | LRMediate Verdict
  | LRConsume
  | LRContest
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.unstableMakeIsData ''LogicRedeemer

{-# INLINEABLE trustProportion #-}
trustProportion :: Proportion -> Integer -> (Integer, Integer)
trustProportion (Proportion acr acd) t = (accuserAmt, accusedAmt)
  where
    rTrust :: R.Rational
    rTrust = R.fromInteger t

    total :: Integer
    total = acr + acd

    accuserAmt :: Integer
    accuserAmt = R.round $ rTrust * (acr R.% total)

    accusedAmt :: Integer
    accusedAmt = R.round $ rTrust * (acd R.% total)

{-# INLINEABLE validCondition #-}
validCondition :: BuiltinByteString -> Inputs -> Bool
validCondition c inps = c `Map.member` inps

{-# INLINEABLE validConditions #-}
validConditions :: Conditions -> Inputs -> Bool
validConditions (CCondition c) inps = validCondition c inps
validConditions (CAny cs) inps = all (`validConditions` inps) cs
validConditions (CAll cs) inps = all (`validConditions` inps) cs

{-# INLINEABLE validLogicConditions #-}
validLogicConditions :: Logic -> Inputs -> Bool
validLogicConditions log inps = all (`validConditions` inps) (Map.keys log)

{-# INLINEABLE conditionFailed #-}
conditionFailed :: BuiltinByteString -> Inputs -> Verdict -> Bool
conditionFailed qst inps ver = case ( do
                                        expected <- Map.lookup qst inps
                                        actual <- Map.lookup qst ver
                                        return $ expected /= actual
                                    ) of
  Just x -> x
  Nothing -> traceError "Question or verdict not found inside input"

{-# INLINEABLE conditionsFailed #-}
conditionsFailed :: Conditions -> Inputs -> Verdict -> Bool
conditionsFailed (CCondition c) inps ver = conditionFailed c inps ver
conditionsFailed (CAny cs) inps ver = any (\c -> conditionsFailed c inps ver) cs
conditionsFailed (CAll cs) inps ver = all (\c -> conditionsFailed c inps ver) cs

{-# INLINEABLE failedProportion #-}
failedProportion :: Inputs -> Verdict -> Logic -> Maybe Proportion
failedProportion inps ver log = do
  cds <- find (\cds -> conditionsFailed cds inps ver) (Map.keys log)
  Map.lookup cds log

{-# INLINEABLE isGuilty #-}
isGuilty :: Logic -> Inputs -> Verdict -> Bool
isGuilty log inps ver = any (\cs -> conditionsFailed cs inps ver) (Map.keys log)

{-# INLINEABLE firstValidJudge #-}
firstValidJudge :: [PubKeyHash] -> [Sig] -> Maybe PubKeyHash
firstValidJudge judges sigs = find (`sigIn` sigs) judges

{-# INLINEABLE findLogicDatum #-}
findLogicDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe LogicState
findLogicDatum o f = do
  dh <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromBuiltinData d
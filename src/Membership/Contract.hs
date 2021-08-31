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

module Membership.Contract where

import Control.Monad (Monad (return))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger
  ( Datum (Datum),
    DatumHash,
    POSIXTime,
    POSIXTimeRange,
    PubKeyHash,
    TxInInfo (txInInfoResolved),
    TxInfo (txInfoInputs, txInfoOutputs),
    TxOut (TxOut, txOutValue),
    TxOutRef,
    TxOutTx (txOutTxOut),
    ValidatorHash,
    contains,
    scriptHashAddress,
    to,
    toValidatorHash,
    txOutDatum,
  )
import Ledger.Typed.Scripts as Scripts (ValidatorTypes (..))
import Ledger.Value
  ( AssetClass,
    CurrencySymbol,
    TokenName,
    Value,
    assetClassValueOf,
    flattenValue,
  )
import Membership.Service (Service (..))
import Membership.Signature (sigTokenToUser)
import Membership.Utils (tripleSnd)
import Plutus.Contract as Contract (Contract, utxoAt)
import qualified PlutusTx
import qualified PlutusTx.AssocMap as M
import PlutusTx.Builtins (BuiltinByteString)
import PlutusTx.Prelude
  ( Bool (..),
    Eq (..),
    Integer,
    Maybe (..),
    elem,
    filter,
    find,
    map,
    not,
    null,
    ($),
    (&&),
    (+),
    (.),
    (<$>),
  )
import qualified Prelude

-- Defining alias to better explain each component's function

-- Yes / No question that will be used in the logic
type InputQuestion = BuiltinByteString

-- The validator hash of a script that will distribute the
-- collateral based on the judges inputs
type LogicScriptVH = ValidatorHash

-- A token that will be sent to the guilty party account
type ShameToken = AssetClass

type User = PubKeyHash

data Role = Publisher | Client | Mediator
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Role where
  {-# INLINEABLE (==) #-}
  Publisher == Publisher = True
  Client == Client = True
  Mediator == Mediator = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''Role

data Judges = Judges
  { jPubKeyHashes :: [PubKeyHash], -- The PubKeyHash of those responsible for mediating conflicts
    jPrice :: Integer, -- The price each judge will receive if he accepts
    jMaxDuration :: POSIXTime -- The maximum time a judge will have to make a decision
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Judges where
  {-# INLINEABLE (==) #-}
  Judges pkhs prc md == Judges pkhs' prc' md' =
    pkhs' == pkhs && prc == prc' && md == md'

PlutusTx.unstableMakeIsData ''Judges

data Accusation = Accusation
  { aAccuser :: PubKeyHash,
    aAccused :: PubKeyHash,
    aTime :: POSIXTime -- The time this accusation was sent
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Accusation where
  {-# INLINEABLE (==) #-}
  Accusation acr acd time == Accusation acr' acd' time' =
    acr' == acr && acd == acd' && time == time'

PlutusTx.unstableMakeIsData ''Accusation

data ContractDatum = ContractDatum
  { cdJudges :: Judges,
    cdInputs :: [InputQuestion],
    cdLogicScript :: (LogicScriptVH, ShameToken),
    cdAccusations :: [Accusation],
    cdService :: Service,
    cdRoleMap :: M.Map User Role
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON)

instance Eq ContractDatum where
  {-# INLINEABLE (==) #-}
  (ContractDatum jds inp ls acc svc rm)
    == (ContractDatum jds' inp' ls' acc' svc' rm') =
      jds == jds'
        && inp == inp'
        && ls == ls'
        && acc == acc'
        && svc == svc'
        && rm == rm'

PlutusTx.unstableMakeIsData ''ContractDatum

data ContractRedeemer = CSign | CAccuse PubKeyHash PubKeyHash | CMediate | CCollect
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq ContractRedeemer where
  {-# INLINEABLE (==) #-}
  CSign == CSign = True
  CAccuse acr acd == CAccuse acr' acd' = acr == acr' && acd == acd'
  CMediate == CMediate = True
  CCollect == CCollect = True
  _ == _ = False

PlutusTx.unstableMakeIsData ''ContractRedeemer

data ContractType

instance Scripts.ValidatorTypes ContractType where
  type DatumType ContractType = ContractDatum
  type RedeemerType ContractType = ContractRedeemer

type DigiContract = (ContractDatum, Value)

{-# INLINEABLE digiContract #-}
digiContract :: ContractDatum -> TxOut -> DigiContract
digiContract cd (TxOut _ v _) = (cd, v)

{-# INLINABLE addUser #-}
addUser :: PubKeyHash -> Role -> ContractDatum -> ContractDatum
addUser pkh userRole oldContractDatum =
  ContractDatum
    { cdJudges = cdJudges oldContractDatum,
      cdInputs = cdInputs oldContractDatum,
      cdLogicScript = cdLogicScript oldContractDatum,
      cdAccusations = cdAccusations oldContractDatum,
      cdService = cdService oldContractDatum,
      cdRoleMap = M.insert pkh userRole (cdRoleMap oldContractDatum)
    }

accuseUser :: PubKeyHash -> PubKeyHash -> POSIXTime -> ContractDatum -> ContractDatum
accuseUser accuser accused currentTime oldContractDatum =
  ContractDatum
    { cdJudges = cdJudges oldContractDatum,
      cdInputs = cdInputs oldContractDatum,
      cdLogicScript = cdLogicScript oldContractDatum,
      cdAccusations = Accusation accuser accused currentTime : cdAccusations oldContractDatum,
      cdService = cdService oldContractDatum,
      cdRoleMap = cdRoleMap oldContractDatum
    }

-- Get's the judge that is supposed to mediate a conflict in case there's one
{-# INLINEABLE currentJudge #-}
currentJudge :: CurrencySymbol -> Accusation -> POSIXTimeRange -> DigiContract -> Maybe PubKeyHash
currentJudge cs (Accusation _ _ accTime) curTime (cd, val) = findJudge authenticatedJudges dln
  where
    judges :: [PubKeyHash]
    judges = jPubKeyHashes $ cdJudges cd

    authenticatedJudges :: [PubKeyHash]
    authenticatedJudges = map (sigTokenToUser . tripleSnd) $ filter f (flattenValue val)
      where
        f :: (CurrencySymbol, TokenName, Integer) -> Bool
        f (cs', tn, _) = (cs' == cs) && (sigTokenToUser tn `elem` judges)

    dln :: POSIXTime
    dln = accTime + jMaxDuration (cdJudges cd)

    findJudge :: [PubKeyHash] -> POSIXTime -> Maybe PubKeyHash
    findJudge [] _ = Nothing
    findJudge (x : xs) deadline =
      if to deadline `contains` curTime
        then Just x
        else findJudge xs (deadline + dln)

-- Verifies if a contract datum is in the "initial state"
{-# INLINEABLE isInitial #-}
isInitial :: PubKeyHash -> ContractDatum -> Bool
isInitial pkh (ContractDatum (Judges jds _ _) inp _ acc _ rm) =
  not (null jds)
    && not (null inp)
    && null acc
    && M.toList rm == [(pkh, Publisher)]

-- Get's the first UTxO sitting at a specific contract validator hash
{-# INLINEABLE findContract #-}
findContract :: AssetClass -> ValidatorHash -> Contract w s Text (Maybe (TxOutRef, TxOutTx))
findContract nft contrValHash = do
  utxos <- Map.filter f <$> utxoAt (scriptHashAddress contrValHash)
  return $ case Map.toList utxos of
    [(oref, o)] -> Just (oref, o)
    _ -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) nft == 1

-- Given a transaction output and a function, tries to get a contract datum
{-# INLINEABLE findContractDatum #-}
findContractDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe ContractDatum
findContractDatum o f = do
  dh <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromBuiltinData d

{-# INLINEABLE findContractInputWithValHash #-}
findContractInputWithValHash :: ValidatorHash -> TxInfo -> Maybe TxOut
findContractInputWithValHash vh info = find predicate ((map txInInfoResolved . txInfoInputs) info)
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

-- Searchs for a transaction output of a specific contract validator hash
-- in the inputs list, but only return it if it's unique
{-# INLINEABLE strictFindContractInputWithValHash #-}
strictFindContractInputWithValHash :: ValidatorHash -> TxInfo -> Maybe TxOut
strictFindContractInputWithValHash vh info = case filter predicate ((map txInInfoResolved . txInfoInputs) info) of
  [o] -> Just o
  _ -> Nothing
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

{-# INLINEABLE findContractOutputWithValHash #-}
findContractOutputWithValHash :: ValidatorHash -> TxInfo -> Maybe TxOut
findContractOutputWithValHash vh info = find predicate (txInfoOutputs info)
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

-- Searchs for a transaction output of a specific contract validator hash
-- in the outputs list, but only return it if it's unique
{-# INLINEABLE strictFindContractOutputWithValHash #-}
strictFindContractOutputWithValHash :: ValidatorHash -> TxInfo -> Maybe TxOut
strictFindContractOutputWithValHash vh info = case filter predicate (txInfoOutputs info) of
  [o] -> Just o
  _ -> Nothing
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just vh
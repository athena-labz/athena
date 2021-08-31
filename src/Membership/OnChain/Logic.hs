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

module Membership.OnChain.Logic where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger
  ( PubKeyHash,
    ScriptContext (scriptContextTxInfo),
    TxInfo,
    Validator,
    ValidatorHash,
    pubKeyOutputsAt,
    txSignedBy,
    validatorHash,
  )
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value (AssetClass, assetClassValueOf)
import qualified PlutusTx
import PlutusTx.Builtins (divideInteger)
import PlutusTx.Prelude
    ( Bool(..),
      Integer,
      (&&),
      any,
      ($),
      fst,
      snd,
      traceError,
      traceIfFalse,
      Eq((==)),
      AdditiveSemigroup((+)),
      Ord((>)) )
import qualified Prelude

-- Verdict is a tuple that will be "filled" by the judges executing the logic
-- It could contain any values or not even not exist
-- It's up to the contract creator to create whatever logic structure he prefers
type Verdict = (Bool, Bool, Integer, Integer)

data VerdictInfo = VerdictInfo
  { viVerdict :: !Verdict,
    viAccuser :: !PubKeyHash,
    viAccused :: !PubKeyHash
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.unstableMakeIsData ''VerdictInfo

{-# INLINEABLE distributeLogic #-}
distributeLogic :: Integer -> Bool -> Bool -> Bool -> Bool -> (Integer, Integer)
distributeLogic amt True False True True = (amt, 0)
distributeLogic amt False True _ _ = (0, amt)
distributeLogic amt _ True False _ = (0, amt)
distributeLogic amt _ True _ False = (0, amt)
distributeLogic amt _ _ _ _ = (amt `divideInteger` 2, amt `divideInteger` 2)

{-# INLINEABLE mkLogicValidator #-}
mkLogicValidator :: AssetClass -> [PubKeyHash] -> () -> VerdictInfo -> ScriptContext -> Bool
mkLogicValidator
  ac
  judges
  _
  (VerdictInfo (bkWtn, acsClt, pgsWtn, numOfRds) acr acd)
  ctx =
    traceIfFalse "operation not allowed" (any (txSignedBy info) judges)
      && traceIfFalse "verdict does not match distribution" checkDistribution
    where
      info :: TxInfo
      info = scriptContextTxInfo ctx

      cdtA, cdtB, cdtC, cdtD :: Bool
      cdtA = bkWtn -- Was the book actually written?
      cdtB = acsClt -- Was the client accessible?
      cdtC = pgsWtn > 200 -- Did the book written have more than 200 pages?
      cdtD = numOfRds > 100_000 -- Did the number of readers exceeded 100k?

      realDistribution :: (Integer, Integer)
      realDistribution = case (pubKeyOutputsAt acr info, pubKeyOutputsAt acd info) of
        ([val], [val']) -> (assetClassValueOf val ac, assetClassValueOf val' ac)
        _ -> traceError "more than one output per user"

      totalAmount :: Integer
      totalAmount = fst realDistribution + snd realDistribution

      expectedDistribution :: (Integer, Integer)
      expectedDistribution = distributeLogic totalAmount cdtA cdtB cdtC cdtD

      checkDistribution :: Bool
      checkDistribution = realDistribution == expectedDistribution

data LogicType

instance Scripts.ValidatorTypes LogicType where
  type DatumType LogicType = ()
  type RedeemerType LogicType = VerdictInfo

typedLogicValidator :: AssetClass -> [PubKeyHash] -> Scripts.TypedValidator LogicType
typedLogicValidator ac j =
  Scripts.mkTypedValidator @LogicType
    ($$(PlutusTx.compile [|| mkLogicValidator ||])
      `PlutusTx.applyCode` PlutusTx.liftCode ac
      `PlutusTx.applyCode` PlutusTx.liftCode j
      )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @() @VerdictInfo

logicValidator :: AssetClass -> [PubKeyHash] -> Validator
logicValidator ac j = Scripts.validatorScript $ typedLogicValidator ac j

logicValHash :: AssetClass -> [PubKeyHash] -> Ledger.ValidatorHash
logicValHash ac j = validatorHash (logicValidator ac j)
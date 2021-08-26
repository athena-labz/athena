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

module Membership.Account where

import Ledger
  ( Datum (Datum),
    DatumHash,
    PubKeyHash,
    ScriptContext,
    TxInInfo (txInInfoResolved),
    TxOut (TxOut, txOutValue),
    findOwnInput,
    getContinuingOutputs,
    txOutDatum,
  )
import Ledger.Typed.Scripts as Scripts (ValidatorTypes (..))
import Ledger.Value
  ( AssetClass,
    Value,
    assetClassValue,
    assetClassValueOf,
  )
import Membership.PlatformSettings (PlatformSettings (..))
import Membership.Signature (findSignatures)
import qualified PlutusTx
import PlutusTx.Prelude
  ( AdditiveGroup ((-)),
    AdditiveSemigroup ((+)),
    Bool,
    Eq (..),
    Integer,
    Maybe (..),
    MultiplicativeSemigroup ((*)),
    Ord ((>)),
    filter,
    fst,
    negate,
    snd,
    traceError,
    ($),
    (&&),
    (<>),
  )
import qualified PlutusTx.Ratio as R
import Wallet.Emulator.Wallet ()
import qualified Prelude

-- The datatype that represents the account information on-chain
data AccountDatum = AccountDatum
  { adCAS :: Integer, -- Between 0 and 100,000
    adReviewCredit :: Integer -- Number of DSET tokens the user received from reviews
  }
  -- Accounts should store a list of service NFTs
  -- This NFTs will serve as a way to uniquely identify each service
  -- They should be minted in the moment of the creation of a contract
  deriving (Prelude.Show)

instance Eq AccountDatum where
  {-# INLINEABLE (==) #-}
  (AccountDatum cas rc) == (AccountDatum cas' rc') = cas == cas' && rc == rc'

PlutusTx.unstableMakeIsData ''AccountDatum

initDatum :: AccountDatum
initDatum = AccountDatum 60_000 0

data AccountRedeemer = Create | Sign | Collect PubKeyHash
  deriving (Prelude.Show)

PlutusTx.unstableMakeIsData ''AccountRedeemer

data AccountType

instance Scripts.ValidatorTypes AccountType where
  type DatumType AccountType = AccountDatum
  type RedeemerType AccountType = AccountRedeemer

{-# INLINEABLE userReviewCredit #-}
userReviewCredit :: AssetClass -> AccountDatum -> Value
userReviewCredit ac ad = assetClassValue ac (adReviewCredit ad)

{-# INLINEABLE calculateNewScore #-}
calculateNewScore :: Integer -> R.Rational -> Integer
calculateNewScore oldScore percentage =
  R.round $ R.fromInteger oldScore + percentage * R.fromInteger (100_000 - oldScore)

{-# INLINEABLE platformFees #-}
platformFees :: AssetClass -> Value -> AccountDatum -> Value
platformFees ac v ad = v <> negate (userReviewCredit ac ad)

{-# INLINEABLE applyCAS #-}
applyCAS :: AccountDatum -> AccountDatum
applyCAS (AccountDatum cas rc) = AccountDatum (calculateNewScore cas (5 R.% 100)) rc

{-# INLINEABLE reviewCreditValue #-}
reviewCreditValue :: PlatformSettings -> AccountDatum -> Value
reviewCreditValue ps ad = assetClassValue (psToken ps) (adReviewCredit ad)

{-# INLINEABLE findAccountDatum #-}
findAccountDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe AccountDatum
findAccountDatum o f = do
  dh <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromBuiltinData d

{-# INLINEABLE strictFindOutAndIn #-}
strictFindOutAndIn :: ScriptContext -> (TxOut, TxOut)
strictFindOutAndIn ctx = (ownInput, ownOutput)
  where
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "account input missing"
      Just i -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "expected exactly one account output"

{-# INLINEABLE findOutAndIn #-}
findOutAndIn :: PubKeyHash -> PlatformSettings -> ScriptContext -> (TxOut, TxOut)
findOutAndIn pkh ps ctx = (ownInput, ownOutput)
  where
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "account input missing"
      Just i -> txInInfoResolved i

    f :: TxOut -> Bool
    f (TxOut _ val _) = findSignatures (psSignatureSymbol ps) val == [pkh]

    ownOutput :: TxOut
    ownOutput = case filter f (getContinuingOutputs ctx) of
      [o] -> o
      _ -> traceError "expected exactly one account output"

{-# INLINEABLE sigTokenIn #-}
sigTokenIn :: AssetClass -> ScriptContext -> Bool
sigTokenIn sig ctx = inputHasToken && outputHasToken
  where
    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    inputTokens :: Integer
    inputTokens = assetClassValueOf (txOutValue ownInput) sig

    inputHasToken :: Bool
    inputHasToken = inputTokens > 0

    outputTokens :: Integer
    outputTokens = assetClassValueOf (txOutValue ownOutput) sig

    outputHasToken :: Bool
    outputHasToken = ((inputTokens - outputTokens) == 1) && (outputTokens > 0)
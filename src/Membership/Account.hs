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
    TxOut,
    txOutDatum,
  )
import Ledger.Typed.Scripts as Scripts (ValidatorTypes (..))
import Ledger.Value (AssetClass, Value, assetClassValue)
import Membership.PlatformSettings (PlatformSettings (..))
import qualified PlutusTx
import PlutusTx.Prelude
  ( AdditiveGroup ((-)),
    AdditiveSemigroup ((+)),
    Eq (..),
    Integer,
    Maybe,
    MultiplicativeSemigroup ((*)),
    ($),
    (&&),
  )
import qualified PlutusTx.Ratio as R
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

{-# INLINABLE initDatum #-}
initDatum :: AccountDatum
initDatum = AccountDatum 60_000 0

data AccountRedeemer = ACreate | ASign | ACollect PubKeyHash
  deriving (Prelude.Show)

PlutusTx.unstableMakeIsData ''AccountRedeemer

data AccountType

instance Scripts.ValidatorTypes AccountType where
  type DatumType AccountType = AccountDatum
  type RedeemerType AccountType = AccountRedeemer

-- Given the asset class that represent's the platform token and an account datum,
-- returns the corresponding value of the review credit in DSET
{-# INLINEABLE userReviewCredit #-}
userReviewCredit :: AssetClass -> AccountDatum -> Value
userReviewCredit ac ad = assetClassValue ac (adReviewCredit ad)

{-# INLINEABLE userReviewCredit' #-}
userReviewCredit' :: PlatformSettings -> AccountDatum -> Value
userReviewCredit' ps = userReviewCredit (psToken ps)

-- Given the old CAS score and a percentage, return's the new score
{-# INLINEABLE calculateNewScore #-}
calculateNewScore :: Integer -> R.Rational -> Integer
calculateNewScore oldScore percentage =
  R.round $ R.fromInteger oldScore + percentage * R.fromInteger (100_000 - oldScore)

-- Increases CAS score based on a coefficient of 5% (the current value for contract creation)
{-# INLINEABLE contractCreationCAS #-}
contractCreationCAS :: AccountDatum -> AccountDatum
contractCreationCAS (AccountDatum cas rc) = AccountDatum (calculateNewScore cas (5 R.% 100)) rc

{-# INLINEABLE findAccountDatum #-}
findAccountDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe AccountDatum
findAccountDatum o f = do
  dh <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromBuiltinData d
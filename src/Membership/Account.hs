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

import Control.Monad hiding (fmap)
import qualified Data.Map as Map
import Data.Monoid (Last (..), Monoid (mempty))
import Data.Monoid as M
import Data.Text (Text, pack)
import Ledger hiding (singleton)
import Ledger.Ada as Ada (Ada (getLovelace), fromValue)
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value
import Membership.Contract
import Membership.PlatformSettings (PlatformSettings (..))
import Membership.Sample as S
import Plutus.Contract as Contract
import Plutus.Contract.StateMachine
import qualified PlutusTx
import PlutusTx.Prelude hiding (ByteString, Semigroup (..), check, unless)
import Wallet.Emulator.Wallet ()
import Prelude (Semigroup (..), Show (..), String)
import Membership.Utils
import qualified PlutusTx.Ratio as R

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

{-# INLINEABLE userReviewCredit #-}
userReviewCredit :: AssetClass -> AccountDatum -> Value
userReviewCredit ac ad = assetClassValue ac (adReviewCredit ad)

{-# INLINEABLE platformFees #-}
platformFees :: AssetClass -> Value -> AccountDatum -> Value
platformFees ac v ad = v <> negate (userReviewCredit ac ad)

{-# INLINABLE applyCAS #-}
applyCAS :: AccountDatum -> AccountDatum
applyCAS (AccountDatum cas rc) = AccountDatum (calculateNewScore cas (5 R.% 100)) rc

{-# INLINABLE reviewCreditValue #-}
reviewCreditValue :: PlatformSettings -> AccountDatum -> Value
reviewCreditValue ps ad = assetClassValue (psToken ps) (adReviewCredit ad)

{-# INLINEABLE findAccountDatum #-}
findAccountDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe AccountDatum
findAccountDatum o f = do
  dh <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromBuiltinData d
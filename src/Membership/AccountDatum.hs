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

module Membership.AccountDatum where

import qualified PlutusTx
import PlutusTx.Prelude (Eq (..), Integer, (&&))
import qualified Prelude

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
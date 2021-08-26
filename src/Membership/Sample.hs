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

module Membership.Sample
  ( sampleToken,
    sampleEntranceFee,
    sampleTxFee,
  )
where

import Ledger.Value
  ( AssetClass (AssetClass),
  )
import PlutusTx.Prelude (Integer)

{-# INLINEABLE sampleToken #-}
sampleToken :: AssetClass
sampleToken = AssetClass ("aa", "DSET")

{-# INLINEABLE sampleEntranceFee #-}
sampleEntranceFee :: Integer
sampleEntranceFee = 300_000

{-# INLINEABLE sampleTxFee #-}
sampleTxFee :: Integer
sampleTxFee = 1_000
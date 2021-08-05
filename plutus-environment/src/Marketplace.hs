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

module Marketplace where

import qualified Data.Map as Map
import Ledger (ValidatorHash)
import Plutus.Contracts.Currency as Currency ()
import PlutusTx.Prelude
  ( AdditiveSemigroup ((+)),
    Bool,
    Eq ((==)),
    Maybe (Nothing, Just),
    isJust,
    length,
    ($),
    (.),
  )
import Service (Service)
import Wallet.Emulator.Wallet ()

type Marketplace = Map.Map ValidatorHash Service

insertService :: Marketplace -> ValidatorHash -> Service -> Marketplace
insertService mkt a s = Map.insert a s mkt

removeService :: Marketplace -> ValidatorHash -> Marketplace
removeService mkt a = Map.delete a mkt

getExtraService :: Maybe Marketplace -> Maybe Marketplace -> Maybe Service
getExtraService mInMkt mOutMkt = do
    inMkt <- mInMkt
    outMkt <- mOutMkt
    let extraS = outMkt Map.\\ inMkt
    case Map.toList extraS of
        [(_, s)] -> if
            (length . Map.keys $ outMkt) == ((length . Map.keys $ inMkt) + 1)
            then Just s
            else Nothing
        _   -> Nothing

hasOneExtraService :: Maybe Marketplace -> Maybe Marketplace -> Bool
hasOneExtraService inMkt outMkt = isJust $ getExtraService inMkt outMkt
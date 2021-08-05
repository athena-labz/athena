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

import Helper (valHash)
import Ledger (ValidatorHash)
import Plutus.Contracts.Currency as Currency ()
import PlutusTx.AssocMap
  ( Map,
    delete,
    fromList,
    insert,
    keys,
  )
import PlutusTx.Prelude
  ( AdditiveSemigroup ((+)),
    Bool,
    Eq ((==)),
    length,
    ($),
    (.),
    (&&)
  )
import Service (Service, defService)
import Wallet.Emulator.Wallet ()

type Marketplace = Map ValidatorHash Service

{-# INLINEABLE defMarketplace #-}
defMarketplace :: Marketplace
defMarketplace = singleton (valHash 1) defService

{-# INLINABLE singleton #-}
singleton :: ValidatorHash -> Service -> Marketplace
singleton vh s = fromList [(vh, s)]

{-# INLINEABLE insertService #-}
insertService :: Marketplace -> ValidatorHash -> Service -> Marketplace
insertService mkt a s = insert a s mkt

{-# INLINEABLE removeService #-}
removeService :: Marketplace -> ValidatorHash -> Marketplace
removeService mkt a = delete a mkt

{-# INLINEABLE isValidService #-}
isValidService :: Marketplace -> Marketplace -> ValidatorHash -> Service -> Bool
isValidService inMkt outMkt vh s = insert vh s inMkt == outMkt

{-# INLINEABLE isValidOffer #-}
isValidOffer :: Marketplace -> Marketplace -> ValidatorHash -> Service -> Bool
isValidOffer inMkt outMkt vh s =
  isValidService inMkt outMkt vh s
    && ((length . keys $ inMkt) + 1) == (length . keys $ outMkt)

{-# INLINEABLE isValidRequest #-}
isValidRequest :: Marketplace -> Marketplace -> ValidatorHash -> Service -> Bool
isValidRequest inMkt outMkt vh s =
  isValidService inMkt outMkt vh s
    && (length . keys $ inMkt) == (length . keys $ outMkt)
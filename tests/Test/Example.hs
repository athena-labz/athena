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

module Test.Example where

import Account.Safe.OffChain
import Contract.Safe.OffChain
import Contract.Create
import Control.Monad (void)
import Control.Monad.Freer.Extras as Extras (logError, logInfo)
import Data.Default (Default (..))
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import Data.Text (Text, pack)
import Ledger
import Ledger.Value
import Plutus.Contract.Test (Wallet (Wallet), knownWallet, walletPubKey)
import Plutus.Trace.Emulator as Emulator
import PlutusTx.AssocMap as PlutusMap
import PlutusTx.Prelude
  ( Bool (..),
    BuiltinByteString,
    Either (Left),
    Integer,
    Maybe (Just, Nothing),
    Semigroup ((<>)),
    ($),
    (++),
    (-),
  )
import Test.Sample
import Test.Trace
import Prelude (IO, Show (..), String)

createAccountExample :: EmulatorTrace ()
createAccountExample = do
  let alice :: Wallet
      alice = knownWallet 1

      tkts :: [AssetClass]
      tkts =
        [ assetClass
            (createContractCurrencySymbol sampleContractSettings)
            "create-contract"
        ]

  aHdr <- activateContractWallet alice accountEndpoints

  createAccountTrace aHdr (sampleAccountSettings tkts)

  void $ Emulator.waitNSlots 1

  -- displayAccountTrace
  --   aHdr
  --   (pubKeyHash $ walletPubKey alice)
  --   (sampleAccountSettings tkts)

  -- void $ Emulator.waitNSlots 1

createContractExample :: EmulatorTrace ()
createContractExample = do
  let alice, judge :: Wallet
      alice = knownWallet 1
      judge = knownWallet 7

      tkts :: [AssetClass]
      tkts =
        [ assetClass
            (createContractCurrencySymbol sampleContractSettings)
            "create-contract"
        ]

  createAccountExample

  aHdr <- activateContractWallet alice contractEndpoints

  createContractTrace
    aHdr
    (sampleAccountSettings tkts)
    sampleContractSettings
    ( sampleContractDatum
        (pubKeyHash $ walletPubKey alice)
        0
        [(pubKeyHashAddress $ pubKeyHash $ walletPubKey judge)]
        []
    )
  
  void $ Emulator.waitNSlots 1

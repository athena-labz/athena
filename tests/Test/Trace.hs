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

module Test.Trace where

import Account
import Account.Create
import Account.Safe.OffChain
import Contract
import Contract.Create
import Contract.Safe.OffChain
import Control.Monad (void)
import Control.Monad.Freer.Extras as Extras (logError, logInfo)
import Data.Default (Default (..))
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import Data.Text (Text, pack)
import Ledger
import Ledger.Ada as Ada (lovelaceValueOf)
import Ledger.TimeSlot
import Ledger.Value (AssetClass (AssetClass), assetClass, assetClassValue)
import Plutus.Contract.Test (Wallet (Wallet), knownWallet, walletPubKey)
import Plutus.Trace.Emulator as Emulator
  ( ContractHandle,
    EmulatorConfig (EmulatorConfig),
    EmulatorTrace,
    activateContractWallet,
    callEndpoint,
    observableState,
    runEmulatorTraceIO',
    waitNSlots,
  )
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
import qualified PlutusTx.Ratio as R
import Test.Sample
import Prelude (IO, Show (..), String)
import qualified Prelude

createAccountTrace ::
  ContractHandle () AccountSchema Text ->
  AccountSettings ->
  EmulatorTrace ()
createAccountTrace h sett = do
  callEndpoint @"create-account" h sett

displayAccountTrace ::
  ContractHandle () AccountSchema Text ->
  PubKeyHash ->
  AccountSettings ->
  EmulatorTrace ()
displayAccountTrace h pkh sett = do
  callEndpoint @"display-account" h (pkh, sett)

createContractTrace ::
  ContractHandle () ContractSchema Text -> 
  AccountSettings ->
  ContractSettings ->
  ContractDatum ->
  EmulatorTrace ()
createContractTrace h aSett cSett cDat = do
  callEndpoint @"create-contract" h (aSett, cSett, cDat)
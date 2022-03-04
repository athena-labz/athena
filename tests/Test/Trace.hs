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

import NFT.OffChain
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
import Plutus.Contract.Test (Wallet (Wallet), knownWallet)
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

mintNFTTrace ::
  ContractHandle () NFTSchema Text ->
  Integer ->
  EmulatorTrace ()
mintNFTTrace h size = do
  callEndpoint @"mint-nft" h size

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
  ContractHandle (Last AssetClass) ContractSchema Text ->
  AccountSettings ->
  ContractSettings ->
  ContractCore ->
  EmulatorTrace (Maybe AssetClass)
createContractTrace h aSett cSett cDat = do
  callEndpoint @"create-contract" h (aSett, cSett, cDat)

  void $ Emulator.waitNSlots 5

  Last mNft <- observableState h
  Prelude.return mNft

signContractTrace ::
  ContractHandle (Last AssetClass) ContractSchema Text ->
  AccountSettings ->
  ContractSettings ->
  Integer ->
  AssetClass ->
  EmulatorTrace ()
signContractTrace h aSett cSett role nft = do
  callEndpoint @"sign-contract" h (aSett, cSett, role, nft)

raiseDisputeTrace ::
  ContractHandle (Last AssetClass) ContractSchema Text ->
  ContractSettings ->
  PubKeyHash ->
  Integer ->
  AssetClass ->
  EmulatorTrace ()
raiseDisputeTrace h cSett acd daysToDln nft = do
  callEndpoint @"raise-dispute" h (cSett, acd, daysToDln, nft)

resolveDisputeTrace ::
  ContractHandle (Last AssetClass) ContractSchema Text ->
  ContractSettings ->
  BuiltinByteString ->
  AssetClass ->
  EmulatorTrace ()
resolveDisputeTrace h cSett vdt nft = do
  callEndpoint @"resolve-dispute" h (cSett, vdt, nft)

consumeCollateralTrace ::
  ContractHandle (Last AssetClass) ContractSchema Text ->
  ContractSettings ->
  Integer ->
  Integer ->
  AssetClass ->
  EmulatorTrace ()
consumeCollateralTrace h cSett perc idx nft = do
  callEndpoint @"consume-collateral" h (cSett, perc, idx, nft)

quitContractTrace ::
  ContractHandle (Last AssetClass) ContractSchema Text ->
  AccountSettings ->
  ContractSettings ->
  AssetClass ->
  EmulatorTrace ()
quitContractTrace h aSett cSett nft = do
  callEndpoint @"quit-contract" h (aSett, cSett, nft)
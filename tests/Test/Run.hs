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

module Test.Run where

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
import PlutusTx.AssocMap as M
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
import Test.Example
import Test.Sample
import Prelude (IO, Show (..), String)
import qualified Prelude

-- An example of an account being created
runCreateAccountExample :: IO ()
runCreateAccountExample = runEmulatorTraceIO' def (abstractConfig [1]) createAccountExample

-- An example of a contract being created
runCreateContractExample :: IO ()
runCreateContractExample = runEmulatorTraceIO' def (abstractConfig [1]) createContractExample

-- An example of a contract being signed
runSignContractExample :: IO ()
runSignContractExample = runEmulatorTraceIO' def (abstractConfig [1, 2]) signContractExample

-- An example of a dispute being raised
runRaiseDisputeExample :: IO ()
runRaiseDisputeExample = runEmulatorTraceIO' def (abstractConfig [1, 2]) raiseDisputeExample

-- An example of a dispute being resolved
runResolveDisputeExample :: IO ()
runResolveDisputeExample = runEmulatorTraceIO' def (abstractConfig [1, 2, 7]) resolveDisputeExample

abstractConfig :: [Integer] -> EmulatorConfig
abstractConfig usrs =
  EmulatorConfig
    (Left $ Map.fromList [(knownWallet w, setValue) | w <- usrs]) def def
  where
    setValue :: Value
    setValue = Ada.lovelaceValueOf 1_000_000_000
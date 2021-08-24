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

module Spec.Trace where

-- ( tests,
--   runMyTrace,
-- )

import Control.Lens
import Control.Monad hiding (fmap)
import Control.Monad.Freer.Extras as Extras
import Data.Default (Default (..))
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import Ledger
import Ledger.Ada as Ada
import Ledger.Value
import Membership.OffChain.Account
import Membership.AccountDatum
import Membership.ContractDatum
import Membership.PlatformSettings
import Membership.Sample
import Membership.Service
import Membership.Utils
import Plutus.Contract.StateMachine
import Plutus.Contract.Test
import Plutus.Trace.Emulator as Emulator
import PlutusTx.Prelude
import Test.Tasty
import Prelude (IO, Show (..), String)

-- tests :: TestTree
-- tests =
--   checkPredicateOptions
--     (defaultCheckOptions & emulatorConfig .~ emCfg)
--     "token sale trace"
--     ( walletFundsChange (Wallet 1) (assetClassValue dset (-30_000))
--         .&&. walletFundsChange (Wallet 2) (assetClassValue dset (-30_000))
--         -- .&&. walletFundsChange (Wallet 3) (Ada.lovelaceValueOf (- 5_000_000) <> assetClassValue token 5)
--     )
--     myTrace

runMyTrace :: IO ()
runMyTrace = runEmulatorTraceIO' def emCfg myTrace

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(Wallet w, setValue w) | w <- [1 .. 3]]) def def
  where
    setValue :: Integer -> Value
    setValue i =
      Ada.lovelaceValueOf 1_000_000_000
        <> assetClassValue sampleToken 1_000_000
        <> assetClassValue (AssetClass (sampleSigSymbol, userToSig (pubKeyHash $ walletPubKey $ Wallet i))) 100

myTrace :: EmulatorTrace ()
myTrace = do
  h <- activateContractWallet (Wallet 1) startEndpoint
  h' <- activateContractWallet (Wallet 2) startEndpoint
  let ps =
        PlatformSettings
          { psVersion = 1,
            psToken = sampleToken,
            psSignatureSymbol = sampleSigSymbol,
            psContractVH = sampleContractHash,
            psEntranceFee = sampleEntranceFee,
            psTxFee = sampleTxFee,
            psCollectors = [pubKeyHash $ walletPubKey $ Wallet 2],
            psOldVH = Nothing
          }
  callEndpoint @"start" h ps
  callEndpoint @"start" h' ps
  void $ Emulator.waitNSlots 5

  h1 <- activateContractWallet (Wallet 1) createEndpoint
  h2 <- activateContractWallet (Wallet 2) createEndpoint
  callEndpoint @"create" h1 (ps, sampleContract)
  callEndpoint @"create" h2 (ps, sampleContract)
  void $ Emulator.waitNSlots 5

  Last m <- observableState h1
  Last m' <- observableState h2

  case (m, m') of
    (Just ac, Just ac') -> do
      Extras.logInfo $ "Contract created " ++ show ac
      Extras.logInfo $ "Contract created " ++ show ac'

      hA <- activateContractWallet (Wallet 1) signEndpoint
      hB <- activateContractWallet (Wallet 2) signEndpoint

      callEndpoint @"sign" hA (ps, ac')
      callEndpoint @"sign" hB (ps, ac)

      void $ Emulator.waitNSlots 5

      hC <- activateContractWallet (Wallet 2) collectEndpoint
      callEndpoint @"collect" hC ps

      void $ Emulator.waitNSlots 5

    _ -> Extras.logError @String "error creating contract"

-- myTrace :: EmulatorTrace ()
-- myTrace = do
--   h <- activateContractWallet (Wallet 1) startEndpoint
--   h' <- activateContractWallet (Wallet 2) startEndpoint
--   let ps =
--         PlatformSettings
--           { psVersion = 1,
--             psToken = sampleToken,
--             psSignatureSymbol = sampleSigSymbol,
--             psContractVH = sampleLogicHash,
--             psEntranceFee = sampleEntranceFee,
--             psTxFee = sampleTxFee,
--             psCollectors = [pubKeyHash $ walletPubKey $ Wallet 2],
--             psOldVH = Nothing
--           }
--   callEndpoint @"start" h ps
--   callEndpoint @"start" h' ps
--   void $ Emulator.waitNSlots 5
--   Last m <- observableState h
--   Last m' <- observableState h'

--   case (m, m') of
--     (Just s, Just s') -> do
--       Extras.logInfo $ "started account" ++ show (ttCurrencySymbol s)
--       Extras.logInfo $ "started account" ++ show (ttCurrencySymbol s')

--       h1 <- activateContractWallet (Wallet 1) createEndpoint
--       h2 <- activateContractWallet (Wallet 2) signEndpoint

--       callEndpoint @"create" h1 (s, ps, sampleContract)
--       void $ Emulator.waitNSlots 5

--       callEndpoint @"sign" h2 (s', pubKeyHash $ walletPubKey $ Wallet 1, ps, sampleContract)
--       void $ Emulator.waitNSlots 5
--     _ -> Extras.logError @String "error starting platform"

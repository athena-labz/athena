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

import Control.Monad (void)
import Control.Monad.Freer.Extras as Extras (logError, logInfo)
import Data.Default (Default (..))
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import Ledger (Value, pubKeyHash)
import Ledger.Ada as Ada (lovelaceValueOf)
import Ledger.Value (AssetClass (AssetClass), assetClassValue)
-- import Membership.Account
-- import Membership.Contract
-- import Membership.OffChain.Account

-- import Membership.Sample
-- import Membership.Service
-- import Membership.Signature
-- import Membership.Utils

import Membership.OffChain.Account
import Membership.OffChain.Signature
import Membership.OnChain.Contract
import Membership.Contract
import Membership.PlatformSettings
  ( IncompletePlatformSettings
      ( IncompletePlatformSettings,
        ipsCollectors,
        ipsContractVH,
        ipsEntranceFee,
        ipsOldVH,
        ipsToken,
        ipsTxFee,
        ipsVersion
      ),
    PlatformSettings
      ( PlatformSettings,
        psCollectors,
        psContractVH,
        psEntranceFee,
        psOldVH,
        psSignatureSymbol,
        psToken,
        psTxFee,
        psVersion
      ),
    sampleEntranceFee,
    sampleTxFee,
  )
import Plutus.Contract.Test (Wallet (Wallet), walletPubKey)
import Plutus.Trace.Emulator as Emulator
  ( EmulatorConfig (EmulatorConfig),
    EmulatorTrace,
    activateContractWallet,
    callEndpoint,
    observableState,
    runEmulatorTraceIO',
    waitNSlots,
  )
import PlutusTx.Prelude
  ( Either (Left),
    Integer,
    Maybe (Just, Nothing),
    Semigroup ((<>)),
    ($),
    (++),
  )
import Test.Tasty ()
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

runMintingTrace :: IO ()
runMintingTrace = runEmulatorTraceIO' def emCfg mintingTrace

emCfg :: EmulatorConfig
emCfg = EmulatorConfig (Left $ Map.fromList [(Wallet w, setValue w) | w <- [1 .. 3]]) def def
  where
    setValue :: Integer -> Value
    setValue _ =
      Ada.lovelaceValueOf 1_000_000_000
        <> assetClassValue (AssetClass ("aa", "DSET")) 1_000_000

mintingTrace :: EmulatorTrace ()
mintingTrace = do
  h1 <- activateContractWallet (Wallet 1) endpoints
  h2 <- activateContractWallet (Wallet 2) endpoints

  let ips =
        IncompletePlatformSettings
          { ipsVersion = 1,
            ipsToken = AssetClass ("aa", "DSET"),
            ipsContractVH = sampleContractHash,
            ipsEntranceFee = sampleEntranceFee,
            ipsTxFee = 1_000,
            ipsCollectors = [pubKeyHash $ walletPubKey (Wallet 1)],
            ipsOldVH = Nothing
          }

  callEndpoint @"mint" h1 ips
  callEndpoint @"mint" h2 ips
  void $ Emulator.waitNSlots 1

myTrace :: EmulatorTrace ()
myTrace = do
  h <- activateContractWallet (Wallet 1) endpoints
  h' <- activateContractWallet (Wallet 2) endpoints

  let ips =
        IncompletePlatformSettings
          { ipsVersion = 1,
            ipsToken = AssetClass ("aa", "DSET"),
            ipsContractVH = sampleContractHash,
            ipsEntranceFee = sampleEntranceFee,
            ipsTxFee = 1_000,
            ipsCollectors = [pubKeyHash $ walletPubKey (Wallet 1)],
            ipsOldVH = Nothing
          }

  callEndpoint @"mint" h ips
  callEndpoint @"mint" h' ips
  void $ Emulator.waitNSlots 5

  Last mPs <- observableState h

  case mPs of
    Just ps -> do
      h1 <- activateContractWallet (Wallet 1) createEndpoint
      h2 <- activateContractWallet (Wallet 2) createEndpoint

      callEndpoint @"create" h1 (ps, sampleContract)
      callEndpoint @"create" h2 (ps, sampleContract)

      void $ Emulator.waitNSlots 5

      Last mConID <- observableState h1
      Last mConID' <- observableState h2

      case (mConID, mConID') of
        (Just ac, Just ac') -> do
          Extras.logInfo $ "Contract created " ++ show ac
          Extras.logInfo $ "Contract created " ++ show ac'

          hA <- activateContractWallet (Wallet 1) signEndpoint
          hB <- activateContractWallet (Wallet 2) signEndpoint

          callEndpoint @"sign" hA (ps, ac')
          callEndpoint @"sign" hB (ps, ac)

          void $ Emulator.waitNSlots 5

          hC <- activateContractWallet (Wallet 1) collectEndpoint
          callEndpoint @"collect" hC ps

          void $ Emulator.waitNSlots 5
        _ -> Extras.logError @String "error creating contract"


    Nothing -> Extras.logError @String "error creating account"

-- myTrace :: EmulatorTrace ()
-- myTrace = do
--   h <- activateContractWallet (Wallet 1) startEndpoint
--   h' <- activateContractWallet (Wallet 2) startEndpoint
--   let ps =
--         PlatformSettings
--           { psVersion = 1,
--             psToken = (AssetClass ("aa", "DSET")),
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

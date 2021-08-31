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

import Control.Monad (void)
import Control.Monad.Freer.Extras as Extras (logError, logInfo)
import Data.Default (Default (..))
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import Ledger
import Ledger.Ada as Ada (lovelaceValueOf)
import Ledger.TimeSlot
import Ledger.Value (AssetClass (AssetClass), assetClass, assetClassValue)
import Membership.Contract
import Membership.OffChain.Account
import Membership.OffChain.Signature
import Membership.OnChain.Contract
import Membership.OnChain.Logic
import Membership.OnChain.Signature
import Membership.PlatformSettings
import Membership.Service
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
import PlutusTx.AssocMap as M
import PlutusTx.Prelude
  ( Either (Left),
    Integer,
    Maybe (Just, Nothing),
    Semigroup ((<>)),
    ($),
    (++),
    (-),
  )
import Test.Tasty ()
import Prelude (IO, Show (..), String)

{-# INLINEABLE sampleService #-}
sampleService :: Service
sampleService =
  Service
    { sPublisher = pubKeyHash $ walletPubKey $ Wallet 1,
      sTitle = "Title",
      sDescription = "Description",
      sTrust = 50_000,
      sType = CConstant
    }

{-# INLINEABLE beginningOfTime #-}
beginningOfTime :: Integer
beginningOfTime = 1596059091000

{-# INLINEABLE platformToken #-}
platformToken :: AssetClass
platformToken = AssetClass ("aa", "DSET")

{-# INLINEABLE sampleEntranceFee #-}
sampleEntranceFee :: Integer
sampleEntranceFee = 300_000

{-# INLINEABLE sampleTxFee #-}
sampleTxFee :: Integer
sampleTxFee = 1_000

{-# INLINEABLE judges #-}
judges :: Judges
judges =
  Judges
    { jPubKeyHashes = [pubKeyHash $ walletPubKey $ Wallet w | w <- [4 .. 10]],
      jPrice = 100_000,
      jMaxDuration = slotToBeginPOSIXTime def 20 - POSIXTime beginningOfTime -- 20 slots to complete mediation
    }

{-# INLINEABLE sampleContractSettings #-}
sampleContractSettings :: ContractSettings
sampleContractSettings = ContractSettings
  { csPlatformToken = platformToken,
    csSignatureSymbol = curSymbol (assetClassValue platformToken sampleTxFee)
  }

{-# INLINEABLE contractValidatorHash #-}
contractValidatorHash :: ValidatorHash
contractValidatorHash = validatorHash $ contractValidator sampleContractSettings

{-# INLINEABLE platformSettings #-}
platformSettings :: PlatformSettings
platformSettings = PlatformSettings
  { psVersion = 1,
    psToken = platformToken,
    psEntranceFee = sampleEntranceFee,
    psTxFee = sampleTxFee
  }

{-# INLINEABLE sampleLogic #-}
sampleLogic :: ValidatorHash
sampleLogic = logicValHash platformToken [pubKeyHash $ walletPubKey $ Wallet w | w <- [4 .. 10]]

sampleContractDatum = ContractDatum
  { cdJudges = judges,
    cdInputs =
      [ "Was a book actually written and delivered?",
        "Was the client accessible?",
        "How many pages were written?",
        "How many readers did the book have?"
      ],
    cdLogicScript = (sampleLogic, assetClass "" ""),
    cdAccusations = [],
    cdService = sampleService,
    cdRoleMap = M.fromList [(pubKeyHash $ walletPubKey $ Wallet 1, Publisher)]
  }

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
        <> assetClassValue platformToken 1_000_000

mintingTrace :: EmulatorTrace ()
mintingTrace = do
  h1 <- activateContractWallet (Wallet 1) endpoints
  h2 <- activateContractWallet (Wallet 2) endpoints

  callEndpoint @"mint" h1 platformSettings
  callEndpoint @"mint" h2 platformSettings
  void $ Emulator.waitNSlots 1

myTrace :: EmulatorTrace ()
myTrace = do
  h <- activateContractWallet (Wallet 1) endpoints
  h' <- activateContractWallet (Wallet 2) endpoints

  callEndpoint @"mint" h platformSettings
  callEndpoint @"mint" h' platformSettings
  void $ Emulator.waitNSlots 5

  Last maybeAccountSettings <- observableState h

  case maybeAccountSettings of
    Just accountSettings -> do
      h1 <- activateContractWallet (Wallet 1) accountEndpoint

      callEndpoint @"create" h1 (accountSettings, sampleContractDatum)

      void $ Emulator.waitNSlots 5

      Last mConID <- observableState h1

      case mConID of
        Just contractNFT -> do
          Extras.logInfo $ "Contract created " ++ show contractNFT

          hA <- activateContractWallet (Wallet 1) contractEndpoint
          hB <- activateContractWallet (Wallet 2) contractEndpoint

          callEndpoint @"sign" hB (Client, accountSettings, contractNFT)

          void $ Emulator.waitNSlots 5

          callEndpoint @"accuse" hA (pubKeyHash $ walletPubKey $ Wallet 1, 100_000, accountSettings, contractNFT)

          void $ Emulator.waitNSlots 5

        _ -> Extras.logError @String "error creating contract"
    Nothing -> Extras.logError @String "error creating account"
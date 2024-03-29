{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}

module Main(main, writeCostingScripts) where


import System.Environment
import           Control.Monad                       (void)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON (..), ToJSON (..), genericToJSON, genericParseJSON
                                                     , defaultOptions, Options(..))
import           Data.Default                        (def)
import qualified Data.OpenApi                        as OpenApi
import           Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import           Plutus.Contract                     (ContractError)
import           Plutus.Trace.Emulator.Extract       (writeScriptsTo, ScriptsConfig (..), Command (..))
import           Ledger.Index                        (ValidatorMode(..))
import           Ledger.Crypto                       (PubKeyHash(..))
import           Test.Example
import           Test.Run
import           Test.Sample
import Ledger.Value
import Account
import Account.Create
import Contract
import Deploy

main :: IO ()
main = writeRedeemer
-- main = do
--   res <- readAssetClass "testnet/asset-class.json"
--   case res of
--     Left _ -> print "ahm?"
--     Right ac -> print ac
-- main = do
--   res <- readAccountDatum
--   case res of
--     Left _ -> print "what?"
--     Right ad -> print $ adCAS ad
-- main = writeCreateContractRedeemer
-- main = do
--   result <- writeMintNFT
--   case result of
--     Right () -> print "cool"
--     _ -> print "not cool"
-- main = writeCostingScripts

initAccountDatum :: IO ()
initAccountDatum = do
  args <- getArgs
  case args of
    accountSettingsPath : outPath : [] -> do
      result <- readJSON accountSettingsPath
      case result of
        Left err -> error err
        Right (as :: AccountSettings) ->
          writeJSON
            outPath
            (initDatum (signatureCurrencySymbol as) (casTickets as))
    _ -> error "arguments not provided"

addContractToAccountDatum :: IO ()
addContractToAccountDatum = do
  args <- getArgs
  case args of
    accountDatumPath : contractAssetClassPath : outPath : [] -> do
      result <- readJSON accountDatumPath
      case result of
        Left err -> error err
        Right (ad :: AccountDatum) -> do
          result' <- readJSON contractAssetClassPath
          case result' of
            Left err -> error err
            Right (ctr :: AssetClass) -> do
              writeJSON
                outPath
                (addContractToAccount ad ctr)
    _ -> error "arguments not provided"

changeAccountDatumCAS :: IO ()
changeAccountDatumCAS = do
  args <- getArgs
  case args of
    accountDatumPath : cas : outPath : [] -> do
      result <- readJSON accountDatumPath
      case result of
        Left err -> error err
        Right (ad :: AccountDatum) ->
          writeJSON
            outPath
            (changeAccountCAS ad (read cas :: Integer))
    _ -> error "arguments not provided"

writeCostingScripts :: IO ()
writeCostingScripts = do
  let config = ScriptsConfig { scPath = "/tmp/plutus-costing-outputs/", scCommand = cmd }
      cmd    = Scripts { unappliedValidators = FullyAppliedValidators }
      -- Note: Here you can use any trace you wish.
      -- trace  = createAccountExample
      -- trace = resolveDisputeExample
      trace = mintNFTExample
  (totalSize, exBudget) <- writeScriptsTo config "script" trace (abstractConfig [1..10])
  putStrLn $ "Total size = " <> show totalSize
  putStrLn $ "ExBudget = " <> show exBudget
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}      
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}

module Main(main) where

import           Control.Monad                       (void)
import           Control.Monad.Freer                 (interpret)
import           Control.Monad.IO.Class              (MonadIO (..))
import           Data.Aeson                          (FromJSON (..), ToJSON (..), genericToJSON, genericParseJSON
                                                     , defaultOptions, Options(..))
import           Data.Text                           (Text)
import           Data.Default                        (def)
import qualified Data.OpenApi                        as OpenApi
import           Prettyprinter                       (Pretty (..), viaShow)
import           GHC.Generics                        (Generic)
import           Plutus.Contract                     (Contract, ContractError)
import           Ledger
import           Ledger.Ada
import           Ledger.Value
import           Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), BuiltinHandler(contractHandler))
import qualified Plutus.PAB.Effects.Contract.Builtin as Builtin
import           Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import qualified Plutus.PAB.Simulator                as Simulator
import qualified Plutus.PAB.Webserver.Server         as PAB.Server
import           Account
import           Account.Safe.OffChain               as AccountOffChain
import           Account.Safe.OnChain                as AccountOnChain
import           Plutus.Trace.Emulator.Extract       (writeScriptsTo, ScriptsConfig (..), Command (..))
import           Ledger.Index                        (ValidatorMode(..))
import           Plutus.PAB.Run (runWith)
import           Plutus.PAB.Run.PSGenerator (HasPSTypes (..))
import qualified Language.PureScript.Bridge.SumType as Bridge ( argonaut, equal, genericShow, mkSumType) 

adaAssetClass :: AssetClass
adaAssetClass = assetClass adaSymbol adaToken

sampleAccountSettings :: [AssetClass] -> AccountSettings
sampleAccountSettings tkts =
  AccountSettings
    { casAccValHash = AccountOnChain.accountValidatorHash,
      casToken = adaAssetClass,
      casEntranceFee = 5_000_000,
      casTickets = tkts
    }

main :: IO ()
main = do
    runWith (Builtin.handleBuiltin @StarterContracts)

instance HasPSTypes StarterContracts where
    psTypes =
        [ Bridge.equal . Bridge.genericShow . Bridge.argonaut $ Bridge.mkSumType @StarterContracts ]

data StarterContracts =
    AccountContract
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass OpenApi.ToSchema

-- NOTE: Because 'StarterContracts' only has one constructor, corresponding to
-- the demo 'Game' contract, we kindly ask aeson to still encode it as if it had
-- many; this way we get to see the label of the contract in the API output!
-- If you simple have more contracts, you can just use the anyclass deriving
-- statement on 'StarterContracts' instead:
--
--    `... deriving anyclass (ToJSON, FromJSON)`
instance ToJSON StarterContracts where
  toJSON = genericToJSON defaultOptions {
             tagSingleConstructors = True }
instance FromJSON StarterContracts where
  parseJSON = genericParseJSON defaultOptions {
             tagSingleConstructors = True }

instance Pretty StarterContracts where
    pretty = viaShow

instance Builtin.HasDefinitions StarterContracts where
    getDefinitions = [AccountContract]
    getSchema =  \case
        AccountContract -> Builtin.endpointsToSchemas @AccountOffChain.AccountSchema
    getContract = \case
        AccountContract -> SomeBuiltin (AccountOffChain.accountEndpoints @ContractError) 









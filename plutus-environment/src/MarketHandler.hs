{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module MarketHandler where

import           Control.Monad             hiding (fmap)
import           Data.Aeson                (FromJSON, ToJSON)
import qualified Data.Map                  as Map
import           Data.Monoid               (Last (..))
import           Data.Text                 (Text, pack)
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           GHC.Generics              (Generic)
import           Plutus.Contract           as Contract
import qualified PlutusTx
import           PlutusTx.Prelude          hiding (Semigroup(..), unless)
import           Ledger                    hiding (singleton)
import           Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada
import           Service
import           Marketplace
import           Plutus.Contracts.Currency as Currency
import           Prelude                   (Semigroup (..), Show (..), String)
import qualified Prelude


data MarketHandler = MarketHandler
    { mSymbol       :: !CurrencySymbol -- Our NFT identifier symbol
    , mMinimumValue :: !Value
    , mPlatformFees :: !Integer -- A percentage of how much should be paid in fees (0 to 1,000)
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''MarketHandler

-- Offer: Publish a service
-- Request: Request a service
-- Distribute: Accusation Contract distribute the tokens accordingly
-- Collect: Collect the platform fees (Should be made in a decentralised way)
data MarketRedeemer = Offer | Request | Distribute | Collect
    deriving Show

PlutusTx.unstableMakeIsData ''MarketRedeemer

{-# INLINABLE marketTokenName #-}
marketTokenName :: TokenName
marketTokenName = TokenName emptyByteString

{-# INLINABLE marketAsset #-}
marketAsset :: MarketHandler -> AssetClass
marketAsset marketplace = AssetClass (mSymbol marketplace, marketTokenName)

{-# INLINABLE marketDatum #-}
marketDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Marketplace
marketDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-
* Offer:
    - Does the datum have exactly one extra service?
    - Was this transaction signed by the publisher of this service?
    - Are the title and description within the character limits?
    - Was the "trust" amount of tokens provided?
    - Is the state "Available"?
    TODO: - Is the contractAddr unique?

* Request:
    - Does the datum have exactly the same services with one exception
    - Does this service that is different have the same paramater values except the state?
    - Was the "trust" amount of tokens provided?
    - Was the "price" value provided?
    - Is the signature provided valid?

* Distribute
    - Did it provide the signature from the provider of the service it's trying to consume?

! Right now a service can only be offered or requested every 20s, allowing bad
! users to spam useless offers in order to block transactions

* Possible ways to avoid this:
    - Create a different validator script for each user
    - Use the membership fee in our favour, temporarilly blocking transactions of specific members
-}

mkMarketplaceValidator :: MarketHandler -> Marketplace -> MarketRedeemer -> ScriptContext -> Bool
mkMarketplaceValidator mkt inputServices r ctx =
    traceIfFalse "token missing from input"  inputHasToken  &&
    traceIfFalse "token missing from output" outputHasToken &&
    case r of
        Offer      -> traceIfFalse "invalid output datum"     oneServiceAdded     &&
                      traceIfFalse "service provided invalid" validOfferService
        Request    -> traceIfFalse "invalid output datum"     noServiceAdded      &&
                      traceIfFalse "service provided invalid" validRequestService
        Distribute -> False
        Collect    -> False
  where
    info :: ScriptContext
    info = scriptContextTxInfo ctx

    -- Get's the marketplace script input from this transaction
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "marketplace input missing"
        Just i  -> txInInfoResolved i

    -- Verifies if the input contains our identity NFT
    inputHasToken :: Bool
    inputHasToken = assetClassValueOf (txOutValue ownInput) (marketAsset mkt) == 1

    -- Get's our output (should be only one)
    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _   -> traceError "expected exactly one marketplace output"

    -- Verifies if the output contains our identity NFT
    outputHasToken :: Bool
    outputHasToken = assetClassValueOf (txOutValue ownOutput) (marketAsset mkt) == 1

    -- Get's the list of sevices from our output if there is one
    outputServices :: Maybe [Service]
    outputServices = marketServices ownOutput (`findDatum` info)

    -- Get's our extra service if there is one
    service :: Maybe Service
    service = do
        oServices <- outputServices
        case oServices \\ inputServices of
            [service] -> Just service
            _         -> Nothing

    lengthDifference :: Maybe Integer
    lengthDifference = do
        oServices <- outputServices
        return $ length oServices - length inputServices

    -- Verifies if there is exactly one extra service in the output
    oneServiceAdded :: Bool
    oneServiceAdded = case lengthDifference of
        Just 1 -> True
        _      -> False

    validOfferService :: Bool
    validOfferService = case service of
        Just s  -> txSignedBy info (publisher s)                             &&
                   length (title s) < titleMaxSize                           &&
                   length (description s) < descriptionMaxSize               &&
                   txOutValue ownOutput `geq` txOutValue ownInput <> trust s &&
                   state s == Available
        Nothing -> False

    noServiceAdded :: Bool
    noServiceAdded = case lengthDifference of
        Just 0 -> True
        _      -> False

    validRequestService :: Bool
    validRequestService = case service of
        Just s  -> txOutValue ownOutput `geq` txOutValue ownInput <> trust s <> price s &&
                  state s == Sold
        Nothing -> False

data MarketplaceType
instance Scripts.ValidatorTypes MarketplaceType where
    type instance DatumType MarketplaceType = [Service]
    type instance RedeemerType MarketplaceType = MarketRedeemer

typedMarketplaceValidator :: MarketHandler -> Scripts.TypedValidator MarketplaceType
typedMarketplaceValidator mkt = Scripts.mkTypedValidator @MarketplaceType
    ($$(PlutusTx.compile [|| mkMarketplaceValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode mkt)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @[Service] @MarketRedeemer

marketValidator :: MarketHandler -> Validator
marketValidator = Scripts.validatorScript . typedMarketplaceValidator

marketAddress :: MarketHandler -> Ledger.Address
marketAddress = scriptAddress . typedMarketplaceValidator

data MarketParams = MarketParams
    { mpSymbol       :: !CurrencySymbol
    , mpMinimumValue :: !Value
    , mpPlatformFees :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

startMarketplace :: forall w s. MarketParams -> Contract w s Text MarketHandler
startMarketplace mp = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    msc <- mapError (pack . show) (mintContract pkh [(marketTokenName, 1)] :: Contract w s CurrencyError OneShotCurrency)
    let cs          = Currency.currencySymbol msc
        marketplace = MarketHandler
            { mSymbol       = cs
            , mMinimumValue = mpMinimumValue mp
            , mPlatformFees = mpPlatformFees mp
            }
    logInfo @String $ "started marketplace " ++ show marketplace
    return marketplace

findMarketplace :: forall w s. MarketHandler -> Contract w s Text (Maybe (TxOutRef, TxOutTx, [Service]))
findMarketplace mkt = do
    -- Find the UTxOs with our NFT (should be only one)
    utxos <- Map.filter f <$> utxoAt (marketAddress mkt)
    return $ case Map.toList utxos of
        [(oref, o)] -> do
            services <- marketServices (txOutTxOut o) $ \dh -> Map.lookup dh $ txData $ txOutTxTx o
            return (oref, o, services)
        _           -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) (marketAsset mkt) == 1

{-
* tx:
    - mustSpendScriptOutput: Creates input spending the UTxO provided
        TxOutRef -> Redeemer -> Constraints 
* lookups:
    - unspentOutputs: we must provide the UTxOs that will be spent
    - typedValidatorLookups
    - otherScript
-}

data ServiceParams = ServiceParams
    { spTitle           :: String
    , spDescription     :: String
    , spPrice           :: Value
    , spTrust           :: Value
    , spContractAddress :: String
    , spState           :: ServiceState
    }

offerService :: forall w s. MarketHandler -> ServiceParams -> Contract w s Text ()
offerService mkt serPmt = do
    pkh <- pubKeyHash <$> Contract.ownPubKey -- Get own PubKey
    m   <- findMarketplace mkt
    let serv = Service
            { publisher    = pkh
            , title        = spTitle serPmt
            , description  = spDescription serPmt
            , price        = spPrice serPmt
            , trust        = spTrust serPmt
            , state        = spState serPmt
            }
    case m of
        Just (oref, o, services) -> do
            let outDatum = serv:services -- Add our service to the Datum
                lookups  = Constraints.unspentOutputs (Map.singleton oref o) <>
                           Constraints.typedValidatorLookups (typedMarketplaceValidator mkt) <>
                           Constraints.otherScript (marketValidator mkt)
                tx       = Constraints.mustPayToTheScript outDatum $ assetClassValue (mSymbol mkt) 1       <>
                           Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Offer)
            ledgerTx <- submitTxConstraintsWith @MarketplaceType lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "offered service " ++ show serv
        Nothing -> do
            let outDatum = [serv]
                c        = Constraints.mustPayToTheScript outDatum $ assetClassValue (mSymbol mkt) 1
            ledgerTx <- submitTxConstraints (typedMarketplaceValidator mkt) c
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "offered first service " ++ show serv

{-
* Offer:
    - Does the datum have exactly one extra service?
    - Was this transaction signed by the publisher of this service?
    - Are the title and description within the character limits?
    - Was the "trust" amount of tokens provided?
    - Is the state "Available"?
    TODO: - Is the contractAddr unique?

* Request:
    - Does the datum have exactly the same services with one exception
    - Does this service that is different have the same paramater values except the state?
    - Was the "trust" amount of tokens provided?
    - Was the "price" value provided?
    - Is the signature provided valid?

* Distribute
    - Did it provide the signature from the provider of the service it's trying to consume?

! Right now a service can only be offered or requested every 20s, allowing bad
! users to spam useless offers in order to block transactions

* Possible ways to avoid this:
    - Create a different validator script for each user
    - Use the membership fee in our favour, temporarilly blocking transactions of specific members
-}

requestService :: forall w s. MarketHandler -> ByteString -> Contract w s Text ()
requestService mkt addr = do
    pkh <- pubKeyHash <$> Contract.ownPubKey -- Get own PubKey
    m   <- findMarketplace mkt
    case m of
        Just (oref, o, services) -> do
            let outDatum = serv:services -- Add our service to the Datum
                lookups  = Constraints.unspentOutputs (Map.singleton oref o) <>
                           Constraints.typedValidatorLookups (typedMarketplaceValidator mkt) <>
                           Constraints.otherScript (marketValidator mkt)
                tx       = Constraints.mustPayToTheScript outDatum $ assetClassValue (mSymbol mkt) 1       <>
                           Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Offer)
            ledgerTx <- submitTxConstraintsWith @MarketplaceType lookups tx
            awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "offered service " ++ show serv
        Nothing -> ()

        


-- let c   = Constraints.mustPayToTheScript (trust service) $ assetClassValue (oracleAsset oracle) 1
--     prc = singleton "ff" "DSET" 200
--     tst = singleton "ff" "DSET" 30
--     serv = Service
--                 { publisher    = pkh
--                 , title        = "Title"
--                 , description  = "Description"
--                 , price        = prc
--                 , trust        = tst
--                 , contractAddr = ""
--                 , state        = Available
--                 }

type MarketSchema =
        Endpoint "offer" ()
    .\/ Endpoint "request" ()

endpoints :: Contract () VestingSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>  grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []

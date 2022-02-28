{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Deploy where

import           Cardano.Api hiding    (Address)
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise       (serialise)
import           Data.Aeson            (decode, encode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as SBS
import           Data.Text             (pack)
import           PlutusTx              (Data (..))
import           PlutusTx.Builtins     (BuiltinByteString, builtinDataToData)
import qualified PlutusTx
import qualified Ledger
import           Ledger.Credential
import           Ledger.Ada
import           Ledger.Address
import           Ledger.Value
import           Ledger.Tx.CardanoAPI
import           Ledger.Crypto

import qualified PlutusTx.AssocMap as PlutusMap

import           NFT.NFT
import           Account
import           Account.Create
import           Account.Safe.OnChain
import           Contract
import           Contract.Accuse
import           Contract.Create
import           Contract.Mediate
import           Contract.Sign
import           Contract.Safe.OnChain
import           Test.Sample

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

readJSON :: PlutusTx.FromData a => FilePath -> IO (Either String a)
readJSON file = do
    raw <- LBS.readFile file
    case decode raw of
        Nothing -> return $ Left "Could not find or parse file"
        Just d -> case scriptDataFromJson ScriptDataJsonDetailedSchema d of
            Left err -> return $ Left "Failed to convert from script data to JSON"
            Right sd -> case PlutusTx.fromBuiltinData (fromCardanoScriptData sd) of
                Nothing -> return $ Left "Could not convert from script data to builtin data"
                Just dat -> return $ Right dat

readAccountSettings :: FilePath -> IO (Either String AccountSettings)
readAccountSettings = readJSON

readAccountDatum :: FilePath -> IO (Either String AccountDatum)
readAccountDatum = readJSON

readAssetClass :: FilePath -> IO (Either String AssetClass)
readAssetClass = readJSON

writeValidator :: FilePath -> Ledger.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Ledger.unValidatorScript

writeMintNFT :: IO (Either (FileError ()) ())
writeMintNFT = writeValidator "testnet/nft.plutus" $ nftValidator 16

writeInitAccountDatum :: IO ()
writeInitAccountDatum =
    writeJSON "testnet/account-datum.json"
        $ initDatum (signatureCurrencySymbol $ sampleAccountSettings tkts) tkts

writeAccountSettings :: IO ()
writeAccountSettings =
    writeJSON "testnet/account-settings.json" $ sampleAccountSettings tkts

writeAccusation :: IO ()
writeAccusation =
    writeJSON "testnet/accusation.json" $ 
        Accusation
            "fcf9960515d2ed06acefd8c16345cbf3cf65265ca1abf3c7d26351c9"
            "44d5415cfe04ac964fde31960f2e501f47ad06d90ded35674fb7583e"
            (Ledger.POSIXTime 1645477229000)
            (Ledger.POSIXTime 1645822829000)


writePubKeyHash :: IO ()
writePubKeyHash =
    writeJSON "testnet/pubkeyhash.json"
        ("fcf9960515d2ed06acefd8c16345cbf3cf65265ca1abf3c7d26351c9" :: PubKeyHash)

writeTxOutRef :: IO ()
writeTxOutRef =
    writeJSON "testnet/outref.json" $
        Ledger.TxOutRef
            ("f318a17ffc1bd50afb6d0363a7c66fd2fff184b96e52e09c56eb4c77d7e156b5" :: Ledger.TxId)
            0

writeAssetClass :: IO ()
writeAssetClass =
    writeJSON "testnet/asset-class.json" $
        assetClass
            (nftCurrencySymbol 16)
            "cool_asset------"

writeRelationType :: RelationType -> IO ()
writeRelationType rt = writeJSON "testnet/relation-type.json" rt

writeValue :: IO ()
writeValue =
    writeJSON "testnet/value.json" $
        assetClassValue
            (assetClass
                (nftCurrencySymbol 16)
                "cool_asset------")
            15

writeAddresses :: [PubKeyHash] -> IO ()
writeAddresses pkhs =
    writeJSON "testnet/addresses.json" $
        map (\pkh -> Address (PubKeyCredential pkh) Nothing) pkhs

writeRolesMap :: [(PubKeyHash, Integer)] -> IO ()
writeRolesMap rm =
    writeJSON "testnet/roles-map.json" $
        PlutusMap.fromList rm

writeContractDatum :: IO ()
writeContractDatum = writeJSON "testnet/contract-datum.json" datum
  where
    datum :: ContractDatum
    datum = ContractDatum
      { cdSigSymbol = signatureCurrencySymbol (sampleAccountSettings tkts),
        cdRelationType = RT_Distributed,
        cdPrivacyType = PT_Public,
        cdPublisher = "fcf9960515d2ed06acefd8c16345cbf3cf65265ca1abf3c7d26351c9",
        cdCollateral = lovelaceValueOf 5_000_000, -- Must be positive
        cdTermsHash = "bewaretheidesofmarch",
        cdJudges = judges,
        cdAccusations = accusations,
        cdResolutions = resolutions,
        cdRoles = 1, -- The maximum role index
        cdRoleMap = rolesMap,
        cdTickets = tkts
      }

    judges :: [Address]
    judges =
      map
        (\pkh -> Address (PubKeyCredential pkh) Nothing) 
        [
            "fcf9960515d2ed06acefd8c16345cbf3cf65265ca1abf3c7d26351c9",
            "44d5415cfe04ac964fde31960f2e501f47ad06d90ded35674fb7583e"
        ]

    accusations :: [Accusation]
    accusations =
      [Accusation
        "fcf9960515d2ed06acefd8c16345cbf3cf65265ca1abf3c7d26351c9"
        "44d5415cfe04ac964fde31960f2e501f47ad06d90ded35674fb7583e"
        (Ledger.POSIXTime 1645477229000)
        (Ledger.POSIXTime 1645822829000)]
    
    resolutions :: [(Accusation, BuiltinByteString)]
    resolutions = [(head accusations, "guilty")]

    rolesMap :: PlutusMap.Map PubKeyHash Integer
    rolesMap =
      PlutusMap.fromList
        [
        ("fcf9960515d2ed06acefd8c16345cbf3cf65265ca1abf3c7d26351c9", 0),
        ("44d5415cfe04ac964fde31960f2e501f47ad06d90ded35674fb7583e", 0)
        ]


writeCreateContractRedeemer :: IO ()
writeCreateContractRedeemer =
    writeJSON "testnet/create-contract-redeemer.json" (pkh, ctr1)
  where
    pkh :: PubKeyHash
    pkh = "fcf9960515d2ed06acefd8c16345cbf3cf65265ca1abf3c7d26351c9"

writeSignContractRedeemer :: IO ()
writeSignContractRedeemer =
    writeJSON "testnet/sign-contract-redeemer.json" (pkh, (1 :: Integer), ctr1)
  where
    pkh :: PubKeyHash
    pkh = "fcf9960515d2ed06acefd8c16345cbf3cf65265ca1abf3c7d26351c9"

writeRaiseDisputeRedeemer :: IO ()
writeRaiseDisputeRedeemer =
    writeJSON "testnet/raise-dispute-redeemer.json" (pkh1, pkh2, posix1, posix2)
  where
    pkh1 :: PubKeyHash
    pkh1 = "fcf9960515d2ed06acefd8c16345cbf3cf65265ca1abf3c7d26351c9"

    pkh2 :: PubKeyHash
    pkh2 = "44d5415cfe04ac964fde31960f2e501f47ad06d90ded35674fb7583e"

    posix1 :: Ledger.POSIXTime
    posix1 = Ledger.POSIXTime 1645477229000

    posix2 :: Ledger.POSIXTime
    posix2 = Ledger.POSIXTime 1645822829000

writeResolveDisputeRedeemer :: IO ()
writeResolveDisputeRedeemer =
    writeJSON "testnet/resolve-dispute-redeemer.json" $
        (pkh, ("resolved" :: BuiltinByteString), posix)
  where
    pkh :: PubKeyHash
    pkh = "fcf9960515d2ed06acefd8c16345cbf3cf65265ca1abf3c7d26351c9"

    posix :: Ledger.POSIXTime
    posix = Ledger.POSIXTime 1645477229000

writeAccountValidator :: IO (Either (FileError ()) ())
writeAccountValidator = writeValidator "testnet/account.plutus" $ accountValidator

writeCreateAccountValidator :: IO (Either (FileError ()) ())
writeCreateAccountValidator =
    writeValidator "testnet/create-account.plutus"
        $ signatureValidator (sampleAccountSettings tkts)

writeContractValidator :: IO (Either (FileError ()) ())
writeContractValidator = writeValidator "testnet/contract.plutus" $ contractValidator

writeCreateContractValidator :: IO (Either (FileError ()) ())
writeCreateContractValidator =
    writeValidator "testnet/create-contract.plutus"
        $ createContractValidator sampleContractSettings

writeSignContractValidator :: IO (Either (FileError ()) ())
writeSignContractValidator =
    writeValidator "testnet/sign-contract.plutus"
        $ signContractValidator sampleContractSettings

writeRaiseDisputeValidator :: IO (Either (FileError ()) ())
writeRaiseDisputeValidator =
    writeValidator "testnet/raise-dispute.plutus"
        $ raiseDisputeValidator sampleContractSettings

writeResolveDisputeValidator :: IO (Either (FileError ()) ())
writeResolveDisputeValidator =
    writeValidator "testnet/resolve-dispute.plutus"
        $ resolveDisputeValidator sampleContractSettings

tkts :: [AssetClass]
tkts =
    [ assetClass
        (createContractCurrencySymbol sampleContractSettings)
        "create-contract",
    assetClass
        (signContractCurrencySymbol sampleContractSettings)
        "sign-contract",
    assetClass
        (raiseDisputeCurrencySymbol sampleContractSettings)
        "raise-dispute",
    assetClass
        (raiseDisputeCurrencySymbol sampleContractSettings)
        "resolve-dispute"
    ]

ctr1 :: AssetClass
ctr1 = assetClass cs tn
  where
    cs :: CurrencySymbol
    cs = nftCurrencySymbol 16

    tn :: TokenName
    tn = "contract--------"
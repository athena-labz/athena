{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module NFT.NFT where

import           Data.Aeson           hiding (Value)
import           GHC.Generics
import           Ledger               hiding (singleton)
import           Ledger.Scripts
import           Ledger.Typed.Scripts as Scripts
import           Ledger.Value
import           Plutus.ChainIndex
import qualified PlutusTx
import PlutusTx.Builtins
import qualified PlutusTx.AssocMap    as PlutusMap
import           PlutusTx.Prelude
import qualified PlutusTx.Ratio       as R
import qualified Prelude
import NFT.NFTUtils

{-# INLINEABLE nftPolicyTraceIfFalse #-}
nftPolicyTraceIfFalse :: BuiltinString -> Bool -> Bool
nftPolicyTraceIfFalse msg = traceIfFalse ("NFT Policy - " <> msg)

{-# INLINEABLE nftPolicyTraceError #-}
nftPolicyTraceError :: forall a. BuiltinString -> a
nftPolicyTraceError msg = traceError ("NFT Policy - " <> msg)

{-# INLINEABLE mkNFTPolicy #-}
mkNFTPolicy :: Integer -> TxOutRef -> ScriptContext -> Bool
mkNFTPolicy size oref ctx = 
    nftPolicyTraceIfFalse "UTxO not consumed" hasUTxO &&
    nftPolicyTraceIfFalse "Invalid token name or amount" validMinting
  where
    -- ! TODO: Add UTxO ID (There are some utxos with same ref but different IDs)
    refId :: BuiltinByteString
    refId = case intToBuiltinByteString $ txOutRefIdx oref of
        bbs | lengthOfByteString bbs == 1 -> "0" `appendByteString` bbs
            | lengthOfByteString bbs == 2 -> bbs
        _ -> nftPolicyTraceError "Invalid ref id"
    
    -- The basic information about this transaction
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    validTokenName :: TokenName -> Bool
    validTokenName (TokenName bs) = sliceByteString 0 (size + 2) bs == expected
      where
        expected :: BuiltinByteString
        expected = sliceByteString 0 size (getTxId $ txOutRefId oref) `appendByteString` refId

    validMinting :: Bool
    validMinting = case flattenValue (txInfoMint info) of
      [(cs, tn, amt)] ->
        cs == ownCurrencySymbol ctx && validTokenName tn && amt == 1
      _ -> False

nftPolicy :: Integer -> Scripts.MintingPolicy
nftPolicy size =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkNFTPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode size

nftCurrencySymbol :: Integer -> CurrencySymbol
nftCurrencySymbol = scriptCurrencySymbol . nftPolicy

nftPlutusScript :: Integer -> Script
nftPlutusScript = unMintingPolicyScript . nftPolicy

nftValidator :: Integer -> Validator
nftValidator = Validator . nftPlutusScript
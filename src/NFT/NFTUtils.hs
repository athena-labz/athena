{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module NFT.NFTUtils where

import qualified Cardano.Api.Shelley as Shelley
import Data.Aeson hiding (Value)
import qualified Data.ByteString.Lazy as LazyByteString
import Ledger (TxOutRef(..), TxId(..), TokenName)
import Ledger.Value (TokenName(..))
import qualified PlutusTx
import PlutusTx.Builtins
import PlutusTx.Prelude

{-# INLINABLE intToBuiltinByteString #-}
intToBuiltinByteString :: Integer -> BuiltinByteString
intToBuiltinByteString i
    | i == 0 = "0"
    | i == 1 = "1"
    | i == 2 = "2"
    | i == 3 = "3"
    | i == 4 = "4"
    | i == 5 = "5"
    | i == 6 = "6"
    | i == 7 = "7"
    | i == 8 = "8"
    | i == 9 = "9"
    | otherwise = intToBuiltinByteString (i `divideInteger` 10)
        `appendByteString` intToBuiltinByteString (i `modInteger` 10)

plutusDataToJSON :: PlutusTx.ToData a => a -> LazyByteString.ByteString
plutusDataToJSON = encode . Shelley.scriptDataToJson Shelley.ScriptDataJsonDetailedSchema . Shelley.fromPlutusData . PlutusTx.toData

curSymbolToRedeemer :: BuiltinByteString -> Integer -> LazyByteString.ByteString
curSymbolToRedeemer hash idx = plutusDataToJSON $ TxOutRef (TxId hash) idx

makeTokenName :: BuiltinByteString -> TxOutRef -> TokenName
makeTokenName assetName oref = TokenName $ assetName `appendByteString` parsedOutput
  where
    parsedOutputId :: BuiltinByteString
    parsedOutputId = case intToBuiltinByteString $ txOutRefIdx oref of
      bbs | lengthOfByteString bbs == 1 -> "0" `appendByteString` bbs
          | otherwise -> bbs

    parsedOutput :: BuiltinByteString
    parsedOutput =
      sliceByteString 0 46 (getTxId $ txOutRefId oref)
        `appendByteString` parsedOutputId

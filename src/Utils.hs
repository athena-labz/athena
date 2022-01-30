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

module Utils where

import qualified Cardano.Api.Shelley as Shelley
import Control.Lens hiding (elements)
import Data.Aeson hiding (Value)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Map as HaskellMap
import Ledger hiding (singleton)
import Ledger.Value
import Plutus.ChainIndex
import qualified PlutusTx
import PlutusTx.Prelude

-- Transforms a ValidatorHash into a BuiltinByteString
{-# INLINEABLE unValidatorHash #-}
unValidatorHash :: ValidatorHash -> BuiltinByteString
unValidatorHash vh = case vh of ValidatorHash h -> h

-- Given a ValidatorHash and a TxInfo, tries to find an output inside the inputs from this TxInfo
-- that has the same ValidatorHash as the given one
{-# INLINEABLE findInputWithValHash #-}
findInputWithValHash :: ValidatorHash -> TxInfo -> Maybe TxOut
findInputWithValHash vh info = find predicate ((map txInInfoResolved . txInfoInputs) info)
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

-- Searchs for a transaction output of a specific validator hash
-- in the inputs list, but only return it if it's unique
{-# INLINEABLE strictFindInputWithValHash #-}
strictFindInputWithValHash :: ValidatorHash -> TxInfo -> Maybe TxOut
strictFindInputWithValHash vh info = case filter predicate ((map txInInfoResolved . txInfoInputs) info) of
  [o] -> Just o
  _ -> Nothing
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

-- Given a ValidatorHash and a TxInfo, tries to find an output inside the outputs from this TxInfo
-- that has the same ValidatorHash as the given one
{-# INLINEABLE findOutputWithValHash #-}
findOutputWithValHash :: ValidatorHash -> TxInfo -> Maybe TxOut
findOutputWithValHash vh info = find predicate (txInfoOutputs info)
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

-- Searchs for a transaction output of a specific validator hash
-- in the outputs list, but only return it if it's unique
{-# INLINEABLE strictFindOutputWithValHash #-}
strictFindOutputWithValHash :: ValidatorHash -> TxInfo -> Maybe TxOut
strictFindOutputWithValHash vh info = case filter predicate (txInfoOutputs info) of
  [o] -> Just o
  _ -> Nothing
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

{-# INLINEABLE parseTokenName #-}
parseTokenName :: TokenName -> PubKeyHash
parseTokenName = PubKeyHash . unTokenName

{-# INLINEABLE parsePubKeyHash #-}
parsePubKeyHash :: PubKeyHash -> TokenName
parsePubKeyHash = TokenName . getPubKeyHash

{-# INLINEABLE validTicket #-}
validTicket :: ScriptContext -> BuiltinByteString -> Integer -> Bool
validTicket ctx bbs expAmt = case flattenValue (txInfoMint info) of
  [(cs, tn, amt)] ->
    cs == ownCurrencySymbol ctx && tn == (TokenName bbs) && amt == expAmt
  _ -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

lookupChainIndexDatum :: ChainIndexTx -> DatumHash -> Maybe Datum
lookupChainIndexDatum ciTx dh = HaskellMap.lookup dh (ciTx ^. citxData)

{-# INLINEABLE init #-}
init :: [a] -> [a]
init [x] = []
init (x : xs) = x : init xs
init [] = traceError "init called with empty list"

{-# INLINEABLE last #-}
last :: [a] -> a
last [x] = x
last (_ : xs) = last xs
last [] = traceError "last called with empty list"

plutusDataToJSON :: PlutusTx.ToData a => a -> LazyByteString.ByteString
plutusDataToJSON = encode . Shelley.scriptDataToJson Shelley.ScriptDataJsonDetailedSchema . Shelley.fromPlutusData . PlutusTx.toData

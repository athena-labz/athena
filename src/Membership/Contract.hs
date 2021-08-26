{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Membership.Contract where

import Control.Monad (Monad (return))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger
    ( PubKeyHash,
      Value,
      POSIXTime,
      AssetClass,
      ValidatorHash,
      CurrencySymbol,
      Datum(Datum),
      DatumHash,
      TxOutRef,
      TxOut(TxOut, txOutValue),
      TxOutTx(txOutTxOut),
      pubKeyHash,
      scriptHashAddress,
      toValidatorHash,
      txOutDatum,
      TxInInfo(txInInfoResolved),
      TxInfo(txInfoInputs, txInfoOutputs) )
import Ledger.Value (assetClassValueOf)
import Membership.PlatformSettings (PlatformSettings (..))
import Membership.Service (Service (sPublisher))
import Membership.Signature (findSignatures)
import Plutus.Contract as Contract (Contract, utxoAt)
import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool,
    BuiltinByteString,
    Eq (..),
    Maybe (..),
    Ord ((>)),
    elem,
    filter,
    find,
    length,
    map,
    not,
    null,
    ($),
    (&&),
    (.),
    (<$>),
  )
import Prelude (Show (..))
import Ledger.Typed.Scripts as Scripts ( ValidatorTypes(..) )
import Wallet.Emulator.Wallet ( walletPubKey, Wallet(Wallet) )
import Membership.Logic
import Membership.Service

type Accusation = (PubKeyHash, PubKeyHash)

data ContractDatum = ContractDatum
  { cdJudges :: [PubKeyHash],
    cdInputs :: [BuiltinByteString],
    cdLogicScript :: ValidatorHash,
    cdAccusations :: [(Accusation, POSIXTime)],
    cdService :: Service
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON)

instance Eq ContractDatum where
  {-# INLINEABLE (==) #-}
  (ContractDatum jud inp ls acc svc) == (ContractDatum jud' inp' ls' acc' svc') =
    jud == jud' && inp == inp' && ls == ls' && acc == acc' && svc == svc'

PlutusTx.unstableMakeIsData ''ContractDatum

{-# INLINEABLE sampleContract #-}
sampleContract :: ContractDatum
sampleContract =
  ContractDatum
    { cdJudges = [pubKeyHash $ walletPubKey $ Wallet w | w <- [4 .. 10]],
      cdInputs =
        [ "Was a book actually written and delivered?",
          "Did it have more than 200 pages?",
          "Was the client collaborative, providing any information needed?"
        ],
      cdLogicScript = sampleLogicHash,
      cdAccusations = [],
      cdService = sampleService
    }

type DigiContract = (ContractDatum, Value)

{-# INLINEABLE digiContract #-}
digiContract :: ContractDatum -> TxOut -> DigiContract
digiContract cd (TxOut _ v _) = (cd, v)

{-# INLINEABLE publisherSignedContract #-}
publisherSignedContract :: CurrencySymbol -> DigiContract -> Bool
publisherSignedContract cs (d, v) = publisher `elem` sigTokenUsers
  where
    sigTokenUsers :: [PubKeyHash]
    sigTokenUsers = findSignatures cs v

    publisher :: PubKeyHash
    publisher = sPublisher (cdService d)

{-# INLINEABLE publisherSignedContract' #-}
publisherSignedContract' :: PlatformSettings -> DigiContract -> Bool
publisherSignedContract' ps = publisherSignedContract (psSignatureSymbol ps)

{-# INLINEABLE isInitial #-}
isInitial :: ContractDatum -> Bool
isInitial (ContractDatum jud inp _ acc _) =
  length jud > 5
    && not (null inp)
    && null acc

{-# INLINEABLE findContract #-}
findContract :: AssetClass -> PlatformSettings -> Contract w s Text (Maybe (TxOutRef, TxOutTx))
findContract nft ps = do
  utxos <- Map.filter f <$> utxoAt (scriptHashAddress (psContractVH ps))
  return $ case Map.toList utxos of
    [(oref, o)] -> Just (oref, o)
    _ -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) nft == 1

{-# INLINEABLE findContractDatum #-}
findContractDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe ContractDatum
findContractDatum o f = do
  dh <- txOutDatum o
  Datum d <- f dh
  PlutusTx.fromBuiltinData d

{-# INLINEABLE findContractInputWithValHash #-}
findContractInputWithValHash :: ValidatorHash -> TxInfo -> Maybe TxOut
findContractInputWithValHash vh info = find predicate ((map txInInfoResolved . txInfoInputs) info)
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

{-# INLINEABLE findContractInputWithValHash' #-}
findContractInputWithValHash' :: PlatformSettings -> TxInfo -> Maybe TxOut
findContractInputWithValHash' ps = findContractInputWithValHash (psContractVH ps)

{-# INLINEABLE strictFindContractInputWithValHash #-}
strictFindContractInputWithValHash :: ValidatorHash -> TxInfo -> Maybe TxOut
strictFindContractInputWithValHash vh info = case filter predicate ((map txInInfoResolved . txInfoInputs) info) of
  [o] -> Just o
  _ -> Nothing
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

{-# INLINEABLE strictFindContractInputWithValHash' #-}
strictFindContractInputWithValHash' :: PlatformSettings -> TxInfo -> Maybe TxOut
strictFindContractInputWithValHash' ps = strictFindContractInputWithValHash (psContractVH ps)

{-# INLINEABLE findContractOutputWithValHash #-}
findContractOutputWithValHash :: ValidatorHash -> TxInfo -> Maybe TxOut
findContractOutputWithValHash vh info = find predicate (txInfoOutputs info)
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

{-# INLINEABLE findContractOutputWithValHash' #-}
findContractOutputWithValHash' :: PlatformSettings -> TxInfo -> Maybe TxOut
findContractOutputWithValHash' ps = findContractOutputWithValHash (psContractVH ps)

{-# INLINEABLE strictFindContractOutputWithValHash #-}
strictFindContractOutputWithValHash :: ValidatorHash -> TxInfo -> Maybe TxOut
strictFindContractOutputWithValHash vh info = case filter predicate (txInfoOutputs info) of
  [o] -> Just o
  _ -> Nothing
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

{-# INLINEABLE strictFindContractOutputWithValHash' #-}
strictFindContractOutputWithValHash' :: PlatformSettings -> TxInfo -> Maybe TxOut
strictFindContractOutputWithValHash' ps = strictFindContractOutputWithValHash (psContractVH ps)
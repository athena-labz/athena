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
  ( AssetClass,
    Datum (Datum),
    DatumHash,
    POSIXTime,
    PubKeyHash,
    TxOut (TxOut, txOutValue),
    TxOutRef,
    TxOutTx (txOutTxOut),
    ValidatorHash,
    Value,
    scriptHashAddress,
    txOutDatum,
  )
import Ledger.Value (assetClassValueOf)
import Membership.PlatformSettings (PlatformSettings (..))
import Membership.Service (Service)
import Plutus.Contract as Contract (Contract, utxoAt)
import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool,
    ByteString,
    Eq (..),
    Maybe (..),
    Ord ((>)),
    length,
    not,
    null,
    ($),
    (&&),
    (<$>),
  )
import Wallet.Emulator.Wallet ()
import Prelude (Show (..))

type Accusation = (PubKeyHash, PubKeyHash)

data ContractDatum = ContractDatum
  { cdJudges :: [PubKeyHash],
    cdInputs :: [ByteString],
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

type DigiContract = (ContractDatum, Value)

{-# INLINEABLE digiContract #-}
digiContract :: ContractDatum -> TxOut -> DigiContract
digiContract cd (TxOut _ v _) = (cd, v)

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
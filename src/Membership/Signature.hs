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

module Membership.Signature where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Contexts (TxInfo, txSignedBy)
import Ledger.Crypto (PubKeyHash (..))
import Ledger.Scripts (ValidatorHash (..))
import Ledger.Value as Value
  ( AssetClass (AssetClass),
    CurrencySymbol,
    TokenName (..),
    Value,
    flattenValue,
    singleton,
  )
import Membership.Utils (tripleSnd)
import qualified PlutusTx
import PlutusTx.Prelude
  ( AdditiveSemigroup ((+)),
    Bool,
    BuiltinByteString,
    Eq ((==)),
    Maybe,
    any,
    appendByteString,
    filter,
    find,
    lengthOfByteString,
    map,
    return,
    sliceByteString,
    ($),
    (.),
    (<$>),
  )
import qualified Prelude as P

-- Because we can't know the account validator hash when
-- creating the signature policy, we embed the output script
-- validator hash in the sig token name. Authentication,
-- later on, must verify that the public key corressponds to
-- the user, but also that the embeded validator hash corresponds
-- to the account

-- Sig is a data type that can be created by givin makeSig a token name
-- and serves as an easy way to get the "signature" essential information
data Sig = Sig
  { sTokenName :: !TokenName,
    sUser :: !PubKeyHash,
    sScript :: !ValidatorHash
  }
  deriving (P.Show, Generic, FromJSON, ToJSON, P.Eq)

PlutusTx.unstableMakeIsData ''Sig

{-# INLINEABLE unValidatorHash #-}
unValidatorHash :: ValidatorHash -> BuiltinByteString
unValidatorHash vh = case vh of ValidatorHash h -> h

-- Join two BuiltinByteString corresponding to the user's
-- PubKeyHash and the account ValidatorHash and make that
-- into a TokeName
{-# INLINEABLE makeSigToken #-}
makeSigToken :: PubKeyHash -> ValidatorHash -> TokenName
makeSigToken pkh vh =
  TokenName $ getPubKeyHash pkh `appendByteString` unValidatorHash vh

{-# INLINEABLE makeSig #-}
makeSig :: TokenName -> Sig
makeSig tn = Sig tn pkh vh
  where
    -- The policy output script validator hash and the user
    -- public key hash can be found by slicing the TokenName in two

    bTokenName :: BuiltinByteString
    bTokenName = unTokenName tn

    vh :: ValidatorHash
    vh = ValidatorHash $ sliceByteString 28 (lengthOfByteString (unTokenName tn) + 1) bTokenName

    pkh :: PubKeyHash
    pkh = PubKeyHash $ sliceByteString 0 28 bTokenName

{-# INLINEABLE sig #-}
sig :: PubKeyHash -> ValidatorHash -> Sig
sig pkh = makeSig . makeSigToken pkh

{-# INLINEABLE sigTokenToUser #-}
sigTokenToUser :: TokenName -> PubKeyHash
sigTokenToUser = sUser . makeSig

-- Given the signature currency symbol and a value,
-- tries to find a signature token
{-# INLINEABLE findSignature #-}
findSignature :: CurrencySymbol -> Value -> Maybe Sig
findSignature cs v = do
  (_, tn, _) <- find (\(cs', _, _) -> cs == cs') (flattenValue v)
  return $ makeSig tn

-- Given the signature currency symbol and a value,
-- tries to find all signature tokens
{-# INLINEABLE findSignatures #-}
findSignatures :: CurrencySymbol -> Value -> [Sig]
findSignatures cs v = map (makeSig . tripleSnd) $ filter (\(cs', _, _) -> cs == cs') (flattenValue v)

-- Given the signature currency symbol and a value,
-- tries to find a signature token and returns it's user
{-# INLINEABLE findSignatory #-}
findSignatory :: CurrencySymbol -> Value -> Maybe PubKeyHash
findSignatory cs v = sUser <$> findSignature cs v

-- Given the signature currency symbol and a value,
-- tries to find all signature tokens and return their users
{-# INLINEABLE findSignatories #-}
findSignatories :: CurrencySymbol -> Value -> [PubKeyHash]
findSignatories cs v = map (sigTokenToUser . tripleSnd) $ filter (\(cs', _, _) -> cs == cs') (flattenValue v)

-- Given a transaction context info and a list of
-- public keys, verify if any of those signed this transaction
{-# INLINEABLE anySigned #-}
anySigned :: TxInfo -> [PubKeyHash] -> Bool
anySigned info = any (txSignedBy info)

-- Given a transaction context info and a list of
-- public keys, get the one that signed this transaction
{-# INLINEABLE whoSigned #-}
whoSigned :: TxInfo -> [PubKeyHash] -> Maybe PubKeyHash
whoSigned info = find (txSignedBy info)

-- Make a SIG token asset class
{-# INLINEABLE signatureAssetClass #-}
signatureAssetClass :: CurrencySymbol -> PubKeyHash -> ValidatorHash -> AssetClass
signatureAssetClass cs pkh vh = AssetClass (cs, makeSigToken pkh vh)

-- Make a single SIG token value
{-# INLINEABLE signatureValue #-}
signatureValue :: CurrencySymbol -> PubKeyHash -> ValidatorHash -> Value
signatureValue cs pkh vh = singleton cs (makeSigToken pkh vh) 1
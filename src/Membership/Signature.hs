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
import Membership.PlatformSettings
  ( PlatformSettings (psSignatureSymbol),
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
  )
import qualified Prelude as P

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

{-# INLINEABLE makeSigToken #-}
makeSigToken :: PubKeyHash -> ValidatorHash -> TokenName
makeSigToken pkh vh =
  TokenName $ getPubKeyHash pkh `appendByteString` unValidatorHash vh

{-# INLINEABLE makeSig #-}
makeSig :: TokenName -> Sig
makeSig tn = Sig tn pkh vh
  where
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

{-# INLINEABLE findSignature #-}
findSignature :: CurrencySymbol -> Value -> Maybe PubKeyHash
findSignature cs v = do
  (_, tn, _) <- find (\(cs', _, _) -> cs == cs') (flattenValue v)
  return $ sigTokenToUser tn

{-# INLINEABLE findSignature' #-}
findSignature' :: PlatformSettings -> Value -> Maybe PubKeyHash
findSignature' ps = findSignature (psSignatureSymbol ps)

{-# INLINEABLE findSignatures #-}
findSignatures :: CurrencySymbol -> Value -> [PubKeyHash]
findSignatures cs v = map (sigTokenToUser . tripleSnd) $ filter (\(cs', _, _) -> cs == cs') (flattenValue v)

{-# INLINEABLE findSignatures' #-}
findSignatures' :: PlatformSettings -> Value -> [PubKeyHash]
findSignatures' ps = findSignatures (psSignatureSymbol ps)

{-# INLINEABLE anySigned #-}
anySigned :: TxInfo -> [PubKeyHash] -> Bool
anySigned info = any (txSignedBy info)

{-# INLINEABLE whoSigned #-}
whoSigned :: TxInfo -> [PubKeyHash] -> Maybe PubKeyHash
whoSigned info = find (txSignedBy info)

{-# INLINEABLE signatureAssetClass #-}
signatureAssetClass :: CurrencySymbol -> PubKeyHash -> ValidatorHash -> AssetClass
signatureAssetClass cs pkh vh = AssetClass (cs, makeSigToken pkh vh)

{-# INLINEABLE signatureAssetClass' #-}
signatureAssetClass' :: PlatformSettings -> PubKeyHash -> ValidatorHash -> AssetClass
signatureAssetClass' ps = signatureAssetClass (psSignatureSymbol ps)

{-# INLINEABLE signatureValue #-}
signatureValue :: CurrencySymbol -> PubKeyHash -> ValidatorHash -> Value
signatureValue cs pkh vh = singleton cs (makeSigToken pkh vh) 1

{-# INLINEABLE signatureValue' #-}
signatureValue' :: PlatformSettings -> PubKeyHash -> ValidatorHash -> Value
signatureValue' ps = signatureValue (psSignatureSymbol ps)
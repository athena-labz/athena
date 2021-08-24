{-# LANGUAGE NumericUnderscores #-}

module Membership.Utils where

import Ledger.Address (toValidatorHash)
import Ledger.Contexts
  ( TxInfo (txInfoOutputs),
    TxOut (TxOut),
    txSignedBy,
    txInInfoResolved,
    txInfoInputs
  )
import Ledger.Crypto (PubKeyHash (PubKeyHash, getPubKeyHash))
import Ledger.Value
  ( AssetClass (AssetClass),
    CurrencySymbol,
    TokenName (unTokenName),
    Value,
    assetClassValue,
    flattenValue,
    singleton,
    tokenName,
  )
import Membership.AccountDatum (AccountDatum (..))
import Membership.ContractDatum (DigiContract, ContractDatum (..))
import Membership.PlatformSettings (PlatformSettings (..))
import Membership.Service (Service (..))
import PlutusTx.Prelude
  ( Bool(..),
    Integer,
    Maybe (..),
    any,
    filter,
    find,
    map,
    negate,
    return,
    ($),
    (*),
    (+),
    (-),
    (.),
    (<>),
    (==),
  )
import qualified PlutusTx.Ratio as R
import qualified Prelude ()

{-# INLINEABLE calculateNewScore #-}
calculateNewScore :: Integer -> R.Rational -> Integer
calculateNewScore oldScore percentage =
  R.round $ R.fromInteger oldScore + percentage * R.fromInteger (100_000 - oldScore)

{-# INLINEABLE sigToUser #-}
sigToUser :: TokenName -> PubKeyHash
sigToUser = PubKeyHash . unTokenName

{-# INLINEABLE userToSig #-}
userToSig :: PubKeyHash -> TokenName
userToSig = tokenName . getPubKeyHash

{-# INLINEABLE findSignature #-}
findSignature :: CurrencySymbol -> Value -> Maybe PubKeyHash
findSignature cs v = do
  (_, tn, _) <- find (\(cs', _, _) -> cs == cs') (flattenValue v)
  return $ sigToUser tn

{-# INLINEABLE tripleSnd #-}
tripleSnd :: (a, b, c) -> b
tripleSnd (_, x, _) = x

{-# INLINEABLE findSignatures #-}
findSignatures :: CurrencySymbol -> Value -> [PubKeyHash]
findSignatures cs v = map (sigToUser . tripleSnd) $ filter (\(cs', _, _) -> cs == cs') (flattenValue v)

{-# INLINEABLE anySigned #-}
anySigned :: TxInfo -> [PubKeyHash] -> Bool
anySigned info = any (txSignedBy info)

{-# INLINEABLE whoSigned #-}
whoSigned :: TxInfo -> [PubKeyHash] -> Maybe PubKeyHash
whoSigned info = find (txSignedBy info)

signatureAssetClass :: PlatformSettings -> PubKeyHash -> AssetClass
signatureAssetClass ps pkh = AssetClass (psSignatureSymbol ps, userToSig pkh)

signatureValue :: PlatformSettings -> PubKeyHash -> Value
signatureValue ps pkh = singleton (psSignatureSymbol ps) (userToSig pkh) 1

{-# INLINEABLE userReviewCredit #-}
userReviewCredit :: AssetClass -> AccountDatum -> Value
userReviewCredit ac ad = assetClassValue ac (adReviewCredit ad)

{-# INLINEABLE platformFees #-}
platformFees :: AssetClass -> Value -> AccountDatum -> Value
platformFees ac v ad = v <> negate (userReviewCredit ac ad)

{-# INLINEABLE findInputWithValHash #-}
findInputWithValHash :: PlatformSettings -> TxInfo -> Maybe TxOut
findInputWithValHash ps info = find predicate ((map txInInfoResolved . txInfoInputs) info)
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just (psContractVH ps)

{-# INLINEABLE strictFindInputWithValHash #-}
strictFindInputWithValHash :: PlatformSettings -> TxInfo -> Maybe TxOut
strictFindInputWithValHash ps info = case filter predicate ((map txInInfoResolved . txInfoInputs) info) of
    [o] -> Just o
    _   -> Nothing
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just (psContractVH ps)

{-# INLINEABLE findOutputWithValHash #-}
findOutputWithValHash :: PlatformSettings -> TxInfo -> Maybe TxOut
findOutputWithValHash ps info = find predicate (txInfoOutputs info)
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just (psContractVH ps)
  
{-# INLINEABLE strictFindOutputWithValHash #-}
strictFindOutputWithValHash :: PlatformSettings -> TxInfo -> Maybe TxOut
strictFindOutputWithValHash ps info = case filter predicate (txInfoOutputs info) of
    [o] -> Just o
    _   -> Nothing
  where
    predicate (TxOut addr _ _) = toValidatorHash addr == Just (psContractVH ps)

{-# INLINEABLE publisherSignedContract #-}
publisherSignedContract :: PlatformSettings -> DigiContract -> Bool
publisherSignedContract ps (d, v) = any (== publisher) sigTokenUsers
  where
    sigTokenUsers :: [PubKeyHash]
    sigTokenUsers = findSignatures (psSignatureSymbol ps) v

    publisher :: PubKeyHash
    publisher = sPublisher (cdService d)

{-# INLINABLE assertTrue #-}
assertTrue :: Bool -> Maybe ()
assertTrue True = Just ()
assertTrue False = Nothing

{-# INLINABLE applyCAS #-}
applyCAS :: AccountDatum -> AccountDatum
applyCAS (AccountDatum cas rc) = AccountDatum (calculateNewScore cas (5 R.% 100)) rc

{-# INLINABLE reviewCreditValue #-}
reviewCreditValue :: PlatformSettings -> AccountDatum -> Value
reviewCreditValue ps ad = assetClassValue (psToken ps) (adReviewCredit ad)
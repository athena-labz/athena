{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Membership.OffChain.Utils where

import Control.Lens hiding (elements)
import Control.Monad (forever)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Map as Map
import Data.Monoid as M (Last (Last), Monoid (mconcat))
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Ledger
  ( ChainIndexTxOut,
    Datum (..),
    DatumHash (..),
    PubKeyHash,
    Redeemer (Redeemer),
    TxOut (txOutValue),
    TxOutRef,
    TxOutTx (..),
    ValidatorHash,
    lookupDatum,
    pubKeyHash,
    toTxOut,
    txId,
  )
import Ledger.Constraints as Constraints
  ( ScriptLookups,
    TxConstraints,
    mustBeSignedBy,
    mustPayToOtherScript,
    mustPayToPubKey,
    mustPayToTheScript,
    mustSpendScriptOutput,
    otherScript,
    typedValidatorLookups,
    unspentOutputs,
  )
import Ledger.Typed.Scripts (ValidatorTypes (..))
import Ledger.Value
  ( AssetClass (..),
    CurrencySymbol,
    Value,
    assetClass,
    assetClassValue,
    assetClassValueOf,
    geq,
    singleton,
  )
import Membership.Account
  ( AccountDatum (..),
    AccountOffChainEssentials (..),
    AccountRedeemer (..),
    AccountType,
    addContract,
    addReview,
    contractCreationCAS,
    findAccountDatum,
    removeContract,
    signContractCAS,
  )
import Membership.Contract
import Membership.Logic
import Membership.OnChain.Account
import Membership.OnChain.Logic
import Membership.PlatformSettings
import Membership.Service
import Membership.Signature (findSignatories, findSignatory, makeSigToken)
import Plutus.ChainIndex
import Plutus.Contract as Contract
  ( Contract,
    Endpoint,
    Promise (awaitPromise),
    awaitTxConfirmed,
    currentTime,
    endpoint,
    handleError,
    logError,
    logInfo,
    mapError,
    ownPubKey,
    select,
    submitTxConstraintsWith,
    tell,
    utxosTxOutTxAt,
    type (.\/),
  )
import Plutus.Contracts.Currency as Currency
  ( CurrencyError,
    OneShotCurrency,
    currencySymbol,
    mintContract,
    mintedValue,
  )
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AM
import PlutusTx.Prelude
  ( AdditiveGroup ((-)),
    Bool,
    BuiltinByteString,
    Eq ((==)),
    Integer,
    Maybe (..),
    filter,
    foldr,
    fst,
    length,
    map,
    mempty,
    negate,
    otherwise,
    return,
    snd,
    traceError,
    ($),
    (*),
    (++),
    (.),
    (/=),
    (<),
    (<$>),
    (>),
    (||),
  )
import qualified PlutusTx.Ratio as R
import Wallet.Emulator.Wallet ()
import Prelude (Semigroup (..), Show (..), String, uncurry)
import qualified Prelude as P

data AccountUTxOInfo = AccountUTxOInfo
  { auiReference :: TxOutRef,
    auiOutTx :: (ChainIndexTxOut, ChainIndexTx),
    auiDatum :: AccountDatum,
    auiFees :: Value,
    auiUser :: PubKeyHash
  }
  deriving (P.Show, Generic, FromJSON, ToJSON, P.Eq)

-- Tries to find a datum, based on ChainIndexTx (basically a Tx) and a DatumHash
lookupChainIndexDatum :: ChainIndexTx -> DatumHash -> Maybe Datum
lookupChainIndexDatum ciTx dh = Map.lookup dh (ciTx ^. citxData)

-- Returns all accounts, their Datum, the Value of fees they hold and their owners
findAccounts :: AccountSettings -> Contract w s Text [AccountUTxOInfo]
findAccounts accountSettings = do
  -- All UTxOs located at the account address
  utxos <- utxosTxOutTxAt (accountAddress accountSettings)

  -- Return these UTxOs altered
  return $ foldr filterUTxO [] (Map.toList utxos)
  where
    platformSettings :: PlatformSettings
    platformSettings = asPlatformSettings accountSettings

    sigSymbol :: CurrencySymbol
    sigSymbol = asSignatureSymbol accountSettings

    -- The number of DSET tokens located in a specific transaction output
    dsetTokens :: ChainIndexTxOut -> Integer
    dsetTokens cTxOut =
      assetClassValueOf
        (txOutValue $ toTxOut cTxOut)
        (psToken platformSettings)

    -- The public key hash from a user with a SIG token in this value
    sigPubKeys :: Value -> [PubKeyHash]
    sigPubKeys v = findSignatories sigSymbol v

    getAccountInfo ::
      (TxOutRef, (ChainIndexTxOut, ChainIndexTx)) ->
      Maybe AccountUTxOInfo
    getAccountInfo (oref, (cTxOut, cTxTx)) = do
      pkh <-
        ( case sigPubKeys (txOutValue $ toTxOut cTxOut) of
            [p] -> Just p
            _ -> Nothing
          )
      accountDatum <-
        findAccountDatum (toTxOut cTxOut) (lookupChainIndexDatum cTxTx)

      let reviewCreditValue :: Value
          reviewCreditValue =
            assetClassValue
              (psToken platformSettings)
              (dsetTokens cTxOut - adReviewCredit accountDatum)

          fees :: Value
          fees = txOutValue (toTxOut cTxOut) <> negate reviewCreditValue

      return
        AccountUTxOInfo
          { auiReference = oref,
            auiOutTx = (cTxOut, cTxTx),
            auiDatum = accountDatum,
            auiFees = fees,
            auiUser = pkh
          }

    filterUTxO ::
      (TxOutRef, (ChainIndexTxOut, ChainIndexTx)) ->
      [AccountUTxOInfo] ->
      [AccountUTxOInfo]
    filterUTxO (oref, o) acc = case getAccountInfo (oref, o) of
      Just aui -> aui : acc
      Nothing -> acc

-- Return the UTxO from the account with a PubKeyHash
findAccount ::
  PubKeyHash ->
  AccountSettings ->
  Contract w s Text (Maybe (TxOutRef, (ChainIndexTxOut, ChainIndexTx)))
findAccount user accountSettings = do
  -- All UTxOs located at the account address
  utxos <- Map.filter f <$> utxosTxOutTxAt (accountAddress accountSettings)

  -- Return UTxO if there is only one UTxO in the filtered list
  return $ case Map.toList utxos of
    [(oref, o)] -> Just (oref, o)
    _ -> Nothing
  where
    -- Only return UTxOs that contain a SIG values, whose signatory is the given user
    f :: (ChainIndexTxOut, ChainIndexTx) -> Bool
    f (cTxOut, _) =
      findSignatories
        (asSignatureSymbol accountSettings)
        (txOutValue $ toTxOut cTxOut)
        == [user]

-- Get's the important information about an account based on the account's user
-- and it's settings
getAccountOffChainEssentials ::
  AccountSettings ->
  PubKeyHash ->
  Contract w s Text (Maybe AccountOffChainEssentials)
getAccountOffChainEssentials accountSettings pkh = do
  -- Tries to get the account that belongs to this user
  maybeAccount <- findAccount pkh accountSettings

  case maybeAccount of
    Just (accountReference, accountOutTx) -> do
      let accountOut :: ChainIndexTxOut
          accountOut = fst accountOutTx

          accountTx :: ChainIndexTx
          accountTx = snd accountOutTx

          maybeAccountDatum :: Maybe AccountDatum
          maybeAccountDatum =
            findAccountDatum
              (toTxOut accountOut)
              (lookupChainIndexDatum accountTx)

      case maybeAccountDatum of
        Just accountDatum -> do
          return $
            Just
              AccountOffChainEssentials
                { aoeAccountReference = accountReference,
                  aoeAccountOutTx = (accountOut, accountTx),
                  aoeAccountDatum = accountDatum
                }
        _ -> do
          logError @String "Get Account Off-Chain Essentials - Account Datum not found"
          return Nothing
    _ -> do
      logError @String "Get Account Off-Chain Essentials - Account not found"
      return Nothing

-- Get's the contract important information based on it's NFT identifier asset
-- class and it's settings
getContractOffChainEssentials ::
  AccountSettings ->
  AssetClass ->
  Contract w s Text (Maybe ContractOffChainEssentials)
getContractOffChainEssentials accountSettings contractNFT = do
  -- Tries to get the contract corresponding to this NFT
  maybeContract <- findContract contractNFT contrValHash

  case maybeContract of
    Just (contractReference, contractOutTx) -> do
      let contractOut :: ChainIndexTxOut
          contractOut = fst contractOutTx

          contractTx :: ChainIndexTx
          contractTx = snd contractOutTx

          maybeContractDatum :: Maybe ContractDatum
          maybeContractDatum =
            findContractDatum
              (toTxOut contractOut)
              (lookupChainIndexDatum contractTx)

      case maybeContractDatum of
        Just contractDatum -> do
          return $
            Just
              ContractOffChainEssentials
                { coeContractReference = contractReference,
                  coeContractOutTx = contractOutTx,
                  coeContractDatum = contractDatum,
                  coeContractSettings =
                    ContractSettings
                      { csPlatformSettings = platformSettings,
                        csSignatureSymbol = sigSym
                      },
                  coeContractValidatorHash = contrValHash
                }
        _ -> do
          logError @String "Get Contract Off-Chain Essentials - Contract Datum not found"
          return Nothing
    _ -> do
      logError @String "Get Contract Off-Chain Essentials - Contract not found"
      return Nothing
  where
    contrValHash :: ValidatorHash
    contrValHash = asContractValidatorHash accountSettings

    platformSettings :: PlatformSettings
    platformSettings = asPlatformSettings accountSettings

    sigSym :: CurrencySymbol
    sigSym = asSignatureSymbol accountSettings

getLogicOffChainEssentials ::
  LogicSettings ->
  AssetClass ->
  Contract w s Text (Maybe LogicOffChainEssentials)
getLogicOffChainEssentials logicSettings shameToken = do
  -- The signer public key hash
  pkh <- pubKeyHash <$> Contract.ownPubKey

  -- Tries to get the logic corresponding to the logic creator and this validator hash
  maybeLogic <- findLogic shameToken logValHash

  case maybeLogic of
    Just (logicReference, logicOutTx) -> do
      let logicOut :: ChainIndexTxOut
          logicOut = fst logicOutTx

          logicTx :: ChainIndexTx
          logicTx = snd logicOutTx

          maybeLogicDatum :: Maybe LogicState
          maybeLogicDatum =
            findLogicDatum
              (toTxOut logicOut)
              (lookupChainIndexDatum logicTx)

      case maybeLogicDatum of
        Just logicDatum -> do
          return $
            Just
              LogicOffChainEssentials
                { loeLogicReference = logicReference,
                  loeLogicOutTx = logicOutTx,
                  loeLogicDatum = logicDatum,
                  loeLogicValidatorHash = logValHash
                }
        _ -> do
          logError @String "Get Logic Off-Chain Essentials - Logic Datum not found"
          return Nothing
    _ -> do
      logError @String "Get Logic Off-Chain Essentials - Logic not found"
      return Nothing
  where
    logValHash :: ValidatorHash
    logValHash = logicValHash logicSettings

accountLookups ::
  AccountSettings ->
  TxOutRef ->
  ChainIndexTxOut ->
  ScriptLookups AccountType
accountLookups as oref o =
  Constraints.unspentOutputs (Map.singleton oref o)
    <> Constraints.typedValidatorLookups (typedAccountValidator as)
    <> Constraints.otherScript (accountValidator as)

spendAccount ::
  TxOutRef ->
  AccountRedeemer ->
  TxConstraints (RedeemerType AccountType) (DatumType AccountType)
spendAccount oref r =
  Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData r)

spendContract ::
  TxOutRef ->
  ContractRedeemer ->
  TxConstraints (RedeemerType ContractType) (DatumType ContractType)
spendContract oref r =
  Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData r)
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

module Contract.Safe.OffChain where

import Account
import Account.Create
import Account.Safe.OffChain
import Account.Safe.OnChain
import Contract
import Contract.Accuse
import Contract.Create
import Contract.Safe.OnChain
import Contract.Sign
import Contract.Mediate
import Control.Monad (forever, void)
import qualified Data.Map as HaskellMap
import Data.Monoid (Last (..))
import Data.Text hiding (singleton, last)
import Ledger hiding (singleton)
import Ledger.Constraints as Constraints
import qualified Ledger.Constraints.TxConstraints as Constraints
import Ledger.Contexts (pubKeyHash)
import Ledger.Scripts
import Ledger.Typed.Scripts (ValidatorTypes (..))
import Ledger.Value as Value
import Plutus.ChainIndex.Tx (ChainIndexTx)
import Plutus.Contract as Contract
import Plutus.Contracts.Currency as Currency
import qualified PlutusTx
import qualified PlutusTx.AssocMap as PlutusMap
import PlutusTx.Prelude
import Text.Printf (printf)
import Utils
import qualified Prelude as Haskell
import qualified Plutus.Contract.Request as Contract

{-# INLINABLE dayInMs #-}
dayInMs :: Integer
dayInMs = 86400000

findContract ::
  forall w s.
  AssetClass ->
  Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, ContractDatum))
findContract nft = do
  -- All UTxOs located at the contract address
  utxos <- HaskellMap.filter f <$> utxosTxOutTxAt contractAddress

  -- Return UTxO if there is only one UTxO in the filtered list
  return $ case HaskellMap.toList utxos of
    [(oref, o)] -> do
      let cTxOut :: ChainIndexTxOut
          cTxOut = Haskell.fst o

          cTxTx :: ChainIndexTx
          cTxTx = Haskell.snd o
      dat <- findContractDatum (toTxOut cTxOut) (lookupChainIndexDatum cTxTx)
      return (oref, cTxOut, dat)
    _ -> Nothing
  where
    -- Only return UTxOs that contain a SIG values, whose signatory is the given user
    f :: (ChainIndexTxOut, ChainIndexTx) -> Haskell.Bool
    f (cTxOut, _) = assetClassValueOf (txOutValue $ toTxOut cTxOut) nft Haskell.== 1

createContract ::
  AccountSettings ->
  ContractSettings ->
  ContractCore ->
  Contract (Last AssetClass) ContractSchema Text ()
createContract aSett cSett cCore = do
  -- The public key hash from the user who is trying to create a contract
  pmtPkh <- Contract.ownPaymentPubKeyHash

  let pkh :: PubKeyHash
      pkh = unPaymentPubKeyHash pmtPkh

  contractNFT <-
    mapError
      (pack Haskell.. Haskell.show)
      ( mintContract pmtPkh [("cid", 1)] ::
          Contract w s CurrencyError OneShotCurrency
      )

  m <- findAccount pkh aSett

  tell $ Last $ Just (assetClass (Currency.currencySymbol contractNFT) "cid")

  case m of
    Nothing -> do
      tell $ Last Nothing
      logError @Haskell.String "Failed to create contract: user has no account"
    Just (aRef, aOut, aDat) -> do
      -- Submits the transaction to the blockchain
      tx <- submitTxConstraintsWith @AccountType lookups tx

      -- Tell the user our contract nft
      tell $ Last $ Just nft

      Contract.logInfo
        @Haskell.String
        $ printf "%s successfully created contract" (Haskell.show pkh)
      where
        tktSymbol, sigSymbol :: CurrencySymbol
        tktSymbol = createContractCurrencySymbol cSett
        sigSymbol = signatureCurrencySymbol aSett

        ticket, nft :: AssetClass
        ticket = assetClass tktSymbol "create-contract"
        nft = assetClass (Currency.currencySymbol contractNFT) "cid"

        cDat :: ContractDatum
        cDat =
          ContractDatum
            { cdSigSymbol = sigSymbol,
              cdRelationType = ccRelationType cCore,
              cdPrivacyType = ccPrivacyType cCore,
              cdPublisher = pkh,
              cdCollateral = ccCollateral cCore,
              cdTermsHash = ccTermsHash cCore,
              cdJudges = ccJudges cCore,
              cdAccusations = [],
              cdResolutions = [],
              cdRoles = ccRoles cCore,
              cdRoleMap = ccRoleMap cCore,
              cdTickets = ccTickets cCore
            }

        aDatNew :: AccountDatum
        aDatNew = addContractToAccount aDat nft

        tktVal, sigVal, aVal, cVal :: Value
        tktVal = assetClassValue ticket 1
        sigVal = singleton (adSigSymbol aDat) (parsePubKeyHash pkh) 1
        aVal = txOutValue (toTxOut aOut) <> negate sigVal <> tktVal
        cVal = sigVal <> assetClassValue nft 1 <> cdCollateral cDat

        lookups :: ScriptLookups AccountType
        lookups =
          Constraints.unspentOutputs (HaskellMap.fromList [(aRef, aOut)])
            Haskell.<> Constraints.mintingPolicy (createContractPolicy cSett)
            Haskell.<> Constraints.otherScript contractValidator
            Haskell.<> Constraints.otherScript accountValidator

        tx :: TxConstraints (RedeemerType AccountType) (DatumType AccountType)
        tx =
          Constraints.mustMintValueWithRedeemer
            (Redeemer $ PlutusTx.toBuiltinData (pkh, nft))
            tktVal
            Haskell.<> Constraints.mustSpendScriptOutput
              aRef
              (Redeemer $ PlutusTx.toBuiltinData ticket)
            Haskell.<> Constraints.mustPayToOtherScript
              accountValidatorHash
              (Datum $ PlutusTx.toBuiltinData aDatNew)
              aVal
            Haskell.<> Constraints.mustPayToOtherScript
              contractValidatorHash
              (Datum $ PlutusTx.toBuiltinData cDat)
              cVal

signContract ::
  AccountSettings ->
  ContractSettings ->
  Integer ->
  AssetClass ->
  Contract (Last AssetClass) ContractSchema Text ()
signContract aSett cSett role nft = do
  -- The public key hash from the user who is trying to sign a contract
  pmtPkh <- Contract.ownPaymentPubKeyHash

  let pkh :: PubKeyHash
      pkh = unPaymentPubKeyHash pmtPkh

  m <- findAccount pkh aSett
  m' <- findContract nft

  case (m, m') of
    (Just (aRef, aOut, aDat), Just (cRef, cOut, cDat)) -> do
      -- Submits the transaction to the blockchain
      void $ submitTxConstraintsWith @AccountType lookups tx

      Contract.logInfo @Haskell.String "successfully signed contract"
      where
        tktSymbol, sigSymbol :: CurrencySymbol
        tktSymbol = signContractCurrencySymbol cSett
        sigSymbol = signatureCurrencySymbol aSett

        ticket :: AssetClass
        ticket = assetClass tktSymbol "sign-contract"

        cOutDat :: ContractDatum
        cOutDat = addUserToContract pkh role cDat

        aOutDat :: AccountDatum
        aOutDat = addContractToAccount aDat nft

        tktVal, sigVal, aVal, cVal :: Value
        tktVal = assetClassValue ticket 1
        sigVal = singleton (adSigSymbol aDat) (parsePubKeyHash pkh) 1
        aVal = txOutValue (toTxOut aOut) <> negate sigVal <> tktVal
        cVal =
          txOutValue (toTxOut cOut)
            <> sigVal
            <> cdCollateral cDat
            <> tktVal

        lookups :: ScriptLookups AccountType
        lookups =
          Constraints.unspentOutputs (HaskellMap.fromList [(aRef, aOut), (cRef, cOut)])
            Haskell.<> Constraints.mintingPolicy (signContractPolicy cSett)
            Haskell.<> Constraints.otherScript contractValidator
            Haskell.<> Constraints.otherScript accountValidator

        tx :: TxConstraints (RedeemerType AccountType) (DatumType AccountType)
        tx =
          Constraints.mustMintValueWithRedeemer
            (Redeemer $ PlutusTx.toBuiltinData (pkh, role, nft))
            (tktVal <> tktVal)
            Haskell.<> Constraints.mustSpendScriptOutput
              aRef
              (Redeemer $ PlutusTx.toBuiltinData ticket)
            Haskell.<> Constraints.mustSpendScriptOutput
              cRef
              (Redeemer $ PlutusTx.toBuiltinData ticket)
            Haskell.<> Constraints.mustPayToOtherScript
              accountValidatorHash
              (Datum $ PlutusTx.toBuiltinData aOutDat)
              aVal
            Haskell.<> Constraints.mustPayToOtherScript
              contractValidatorHash
              (Datum $ PlutusTx.toBuiltinData cOutDat)
              cVal
    _ ->
      logError @Haskell.String "Failed to sign contract: account or contract not found"

raiseDispute ::
  ContractSettings ->
  PubKeyHash ->
  Integer ->
  AssetClass ->
  Contract (Last AssetClass) ContractSchema Text ()
raiseDispute cSett acd daysToDln nft = do
  -- The public key hash from the user who is trying to raise a dispute
  pmtPkh <- Contract.ownPaymentPubKeyHash

  let pkh :: PubKeyHash
      pkh = unPaymentPubKeyHash pmtPkh

  -- Current POSIXTime
  time <- Contract.currentTime

  m <- findContract nft

  case m of
    Nothing -> logError @Haskell.String "Failed to raise dispute: contract not found"
    Just (cRef, cOut, cDat) -> do
      -- Submits the transaction to the blockchain
      void $ submitTxConstraintsWith @ContractType lookups tx

      Contract.logInfo @Haskell.String "successfully raised dispute"
      where
        dln :: POSIXTime
        dln = POSIXTime $ (getPOSIXTime time) + (daysToDln * dayInMs)

        tktSymbol :: CurrencySymbol
        tktSymbol = raiseDisputeCurrencySymbol cSett

        ticket :: AssetClass
        ticket = assetClass tktSymbol "raise-dispute"

        cOutDat :: ContractDatum
        cOutDat = addAccusationToContract (Accusation pkh acd time dln) cDat

        tktVal, cVal :: Value
        tktVal = assetClassValue ticket 1
        cVal = txOutValue (toTxOut cOut) <> tktVal

        lookups :: ScriptLookups ContractType
        lookups =
          Constraints.unspentOutputs (HaskellMap.fromList [(cRef, cOut)])
            Haskell.<> Constraints.mintingPolicy (raiseDisputePolicy cSett)
            Haskell.<> Constraints.otherScript contractValidator

        tx :: TxConstraints (RedeemerType ContractType) (DatumType ContractType)
        tx =
          Constraints.mustMintValueWithRedeemer
            (Redeemer $ PlutusTx.toBuiltinData (pkh, acd, time, dln))
            tktVal
            Haskell.<> Constraints.mustSpendScriptOutput
              cRef
              (Redeemer $ PlutusTx.toBuiltinData ticket)
            Haskell.<> Constraints.mustPayToOtherScript
              contractValidatorHash
              (Datum $ PlutusTx.toBuiltinData cOutDat)
              cVal

resolveDispute ::
  ContractSettings ->
  BuiltinByteString ->
  AssetClass ->
  Contract (Last AssetClass) ContractSchema Text ()
resolveDispute cSett verdict nft = do
  -- The public key hash from the user who is trying to resolve a dispute
  pmtPkh <- Contract.ownPaymentPubKeyHash

  let pkh :: PubKeyHash
      pkh = unPaymentPubKeyHash pmtPkh

  m <- findContract nft

  case m of
    Nothing -> logError @Haskell.String "Failed to resolve dispute: contract not found"
    Just (cRef, cOut, cDat) -> do
      -- Submits the transaction to the blockchain
      void $ submitTxConstraintsWith @ContractType lookups tx

      Contract.logInfo @Haskell.String "successfully resolved dispute"
      where
        dln :: POSIXTime
        dln = aDeadline (last (cdAccusations cDat))

        tktSymbol :: CurrencySymbol
        tktSymbol = resolveDisputeCurrencySymbol cSett

        ticket :: AssetClass
        ticket = assetClass tktSymbol "resolve-dispute"

        cOutDat :: ContractDatum
        cOutDat = resolveDisputeInContract verdict cDat

        tktVal, cVal :: Value
        tktVal = assetClassValue ticket 1
        cVal = txOutValue (toTxOut cOut) <> tktVal

        lookups :: ScriptLookups ContractType
        lookups =
          Constraints.unspentOutputs (HaskellMap.fromList [(cRef, cOut)])
            Haskell.<> Constraints.mintingPolicy (raiseDisputePolicy cSett)
            Haskell.<> Constraints.otherScript contractValidator

        tx :: TxConstraints (RedeemerType ContractType) (DatumType ContractType)
        tx =
          Constraints.mustMintValueWithRedeemer
            (Redeemer $ PlutusTx.toBuiltinData (pkh, verdict, dln))
            tktVal
            Haskell.<> Constraints.mustSpendScriptOutput
              cRef
              (Redeemer $ PlutusTx.toBuiltinData ticket)
            Haskell.<> Constraints.mustPayToOtherScript
              contractValidatorHash
              (Datum $ PlutusTx.toBuiltinData cOutDat)
              cVal

type ContractSchema =
  Endpoint "create-contract" (AccountSettings, ContractSettings, ContractCore)
    .\/ Endpoint "sign-contract" (AccountSettings, ContractSettings, Integer, AssetClass)
    .\/ Endpoint "raise-dispute" (ContractSettings, PubKeyHash, Integer, AssetClass)
    .\/ Endpoint "resolve-dispute" (ContractSettings, BuiltinByteString, AssetClass)

contractEndpoints :: Contract (Last AssetClass) ContractSchema Text ()
contractEndpoints =
  forever $
    handleError logError $
      awaitPromise $
        createContract' `select` signContract' `select` raiseDispute'
  where
    createContract' = endpoint @"create-contract" $
      \(aSett, cSett, cDat) -> createContract aSett cSett cDat

    signContract' = endpoint @"sign-contract" $
      \(aSett, cSett, role, nft) -> signContract aSett cSett role nft

    raiseDispute' = endpoint @"raise-dispute" $
      \(cSett, acd, daysToDln, nft) -> raiseDispute cSett acd daysToDln nft
    
    resolveDispute' = endpoint @"resolve-dispute" $
      \(cSett, verdict, nft) -> resolveDispute cSett verdict nft
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
import Contract.Create
import Contract.Safe.OnChain
import Control.Monad (forever, void)
import qualified Data.Map as HaskellMap
import Data.Text hiding (singleton)
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

createContract ::
  AccountSettings ->
  ContractSettings ->
  ContractDatum ->
  Contract () ContractSchema Text ()
createContract aSett cSett cDat = do
  -- The public key hash from the user who is trying to create an account
  pkh <- pubKeyHash <$> Contract.ownPubKey

  contractNFT <-
    mapError
      (pack Haskell.. Haskell.show)
      ( mintContract pkh [("cid", 1)] ::
          Contract w s CurrencyError OneShotCurrency
      )

  m <- findAccount pkh aSett

  case m of
    Nothing ->
      logError @Haskell.String "Failed to create contract: user has no accounts"
    Just (aRef, aOut, aDat) -> do
  
      -- Submits the transaction to the blockchain
      ledgerTx <- submitTxConstraintsWith @AccountType lookups tx

      -- Waits for the transaction to be confirmed
      void $ awaitTxConfirmed $ txId ledgerTx

      Contract.logInfo
        @Haskell.String
        $ printf "%s successfully created account" (Haskell.show pkh)
      where
        tktSymbol, sigSymbol :: CurrencySymbol
        tktSymbol = createContractCurrencySymbol cSett
        sigSymbol = signatureCurrencySymbol aSett

        ticket, nft :: AssetClass
        ticket = assetClass tktSymbol "create-contract"
        nft = assetClass (Currency.currencySymbol contractNFT) "cid"

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

type ContractSchema =
  Endpoint "create-contract" (AccountSettings, ContractSettings, ContractDatum)

contractEndpoints :: Contract () ContractSchema Text ()
contractEndpoints =
  forever $
    handleError logError $
      awaitPromise $
        createContract'
  where
    createContract' = endpoint @"create-contract" $
      \(aSett, cSett, cDat) -> createContract aSett cSett cDat
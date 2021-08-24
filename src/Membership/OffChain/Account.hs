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

module Membership.OffChain.Account where

import Control.Monad hiding (fmap)
import qualified Data.Map as Map
import Data.Monoid (Last (..), Monoid (mempty))
import Data.Monoid as M
import Data.Text (Text, pack)
import Ledger hiding (singleton)
import Ledger.Ada as Ada (Ada (getLovelace), fromValue)
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value
import Membership.AccountDatum
import Membership.AccountRedeemer (AccountRedeemer (..))
import Membership.ContractDatum
import Membership.OnChain.Account
import Membership.PlatformSettings (PlatformSettings (..))
import Membership.Sample as S
import Membership.Utils
import Plutus.Contract as Contract
import Plutus.Contract.StateMachine
import Plutus.Contracts.Currency as Currency
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), check, unless)
import Wallet.Emulator.Wallet ()
import Prelude (Semigroup (..), Show (..), String, uncurry)

findAccount :: PubKeyHash -> PlatformSettings -> Contract w s Text (Maybe (TxOutRef, TxOutTx))
findAccount owner ps = do
  utxos <- Map.filter f <$> utxoAt (accountAddress ps)
  logInfo @String $ "utxos: " ++ show (map fst (Map.toList utxos))
  return $ case Map.toList utxos of
    [(oref, o)] -> Just (oref, o)
    _ -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = findSignatures (psSignatureSymbol ps) (txOutValue $ txOutTxOut o) == [owner]

findContract :: AssetClass -> PlatformSettings -> Contract w s Text (Maybe (TxOutRef, TxOutTx))
findContract nft ps = do
  utxos <- Map.filter f <$> utxoAt (scriptHashAddress (psContractVH ps))
  logInfo @String $ "utxos: " ++ show (map fst (Map.toList utxos))
  return $ case Map.toList utxos of
    [(oref, o)] -> Just (oref, o)
    _ -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = assetClassValueOf (txOutValue $ txOutTxOut o) nft == 1

createAccount :: forall s. PlatformSettings -> Contract () s Text ()
createAccount ps = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  let v =
        assetClassValue (psToken ps) (psEntranceFee ps)
          <> singleton (psSignatureSymbol ps) (userToSig pkh) 100
      tx = Constraints.mustPayToOtherScript (accountValidatorHash ps) (Datum $ PlutusTx.toBuiltinData initDatum) v
  ledgerTx <- submitTxConstraints @AccountType (typedAccountValidator ps) tx
  logInfo @String $ show pkh
  logInfo @String $ show (txData ledgerTx)
  logInfo @String $ "initialised account"

createContract :: PlatformSettings -> ContractDatum -> Contract (Last AssetClass) s Text ()
createContract ps dat = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  m <- findAccount pkh ps
  case m of
    Just (oref, TxOutTx outTx out) -> do
      case findAccountDatum out (lookupDatum outTx) of
        Just aDat -> do
          osc <-
            mapError
              (pack . show)
              (mintContract pkh [("", 1)] :: Contract w s CurrencyError OneShotCurrency)
          let cs = Currency.currencySymbol osc
              cv =
                singleton (psSignatureSymbol ps) (userToSig pkh) 1
                  <> Currency.mintedValue osc
              av =
                txOutValue out
                  <> negate (singleton (psSignatureSymbol ps) (userToSig pkh) 1)
                  <> assetClassValue (psToken ps) (psTxFee ps)
              lookups =
                Constraints.unspentOutputs (Map.singleton oref (TxOutTx outTx out))
                  <> Constraints.typedValidatorLookups (typedAccountValidator ps)
                  <> Constraints.otherScript (accountValidator ps)
              tx =
                Constraints.mustBeSignedBy pkh
                  <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Create)
                  <> Constraints.mustPayToTheScript (applyCAS aDat) av
                  <> Constraints.mustPayToOtherScript
                    (psContractVH ps)
                    (Datum $ PlutusTx.toBuiltinData dat)
                    cv
          ledgerTx <- submitTxConstraintsWith @AccountType lookups tx
          tell $ Last $ Just (AssetClass (cs, ""))
          logInfo @String $ show (AssetClass (cs, ""))
          logInfo @String $ "created contract"
        Nothing -> logError @String "no datum found"
    Nothing -> logError @String "no account found"

signContract :: PlatformSettings -> AssetClass -> Contract w s Text ()
signContract ps ac = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  m <- findAccount pkh ps
  m' <- findContract ac ps
  case (m, m') of
    (Just (oref, TxOutTx outTx o), Just (oref', TxOutTx outTx' o')) -> do
      case (findAccountDatum o (lookupDatum outTx), findContractDatum o' (lookupDatum outTx')) of
        (Just aDat, Just cDat) -> do
          let cv =
                txOutValue o'
                  <> singleton (psSignatureSymbol ps) (userToSig pkh) 1
              av =
                txOutValue o
                  <> negate (singleton (psSignatureSymbol ps) (userToSig pkh) 1)
                  <> assetClassValue (psToken ps) (psTxFee ps)
              lookups =
                Constraints.unspentOutputs
                  ( Map.fromList [(oref, TxOutTx outTx o), (oref', TxOutTx outTx' o')]
                  )
                  <> Constraints.otherScript (accountValidator ps)
                  <> Constraints.otherScript sampleContractVal
                  <> Constraints.typedValidatorLookups sampleTypedValidator
              tx =
                Constraints.mustBeSignedBy pkh
                  <> Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData Sign)
                  <> Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ())
                  <> Constraints.mustPayToTheScript cDat cv
                  <> Constraints.mustPayToOtherScript
                    (accountValidatorHash ps)
                    (Datum $ PlutusTx.toBuiltinData (applyCAS aDat))
                    av
          ledgerTx <- submitTxConstraintsWith @SampleContractType lookups tx
          logInfo @String $ "signed contract"
        _ -> logError @String "account or contract datum not found"
    _ -> logError @String "account or contract not found"

type AccountSchema =
  Endpoint "start" PlatformSettings
    .\/ Endpoint "create" (PlatformSettings, ContractDatum)
    .\/ Endpoint "sign" (PlatformSettings, AssetClass)

startEndpoint :: Contract () AccountSchema Text ()
startEndpoint = forever $
  handleError logError $
    awaitPromise $
      endpoint @"start" $ \ps -> createAccount ps

createEndpoint :: Contract (Last AssetClass) AccountSchema Text ()
createEndpoint =
  forever $
    handleError logError $
      awaitPromise $
        endpoint @"create" $ uncurry createContract

signEndpoint :: Contract () AccountSchema Text ()
signEndpoint =
  forever $
    handleError logError $
      awaitPromise $
        endpoint @"sign" $ uncurry signContract
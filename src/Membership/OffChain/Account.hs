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

import Control.Monad (forever)
import qualified Data.Map as Map
import Data.Monoid as M (Last (Last), Monoid (mconcat))
import Data.Text (Text, pack)
import Ledger
  ( Datum (Datum),
    PubKeyHash,
    Redeemer (Redeemer),
    TxOut (txOutValue),
    TxOutRef,
    TxOutTx (..),
    lookupDatum,
    pubKeyHash,
    txId,
  )
import Ledger.Constraints as Constraints
  ( mustBeSignedBy,
    mustPayToOtherScript,
    mustPayToTheScript,
    mustSpendScriptOutput,
    otherScript,
    typedValidatorLookups,
    unspentOutputs,
  )
import Ledger.Value
  ( AssetClass (AssetClass),
    Value,
    assetClassValue,
    assetClassValueOf,
    singleton,
  )
import Membership.Account
  ( AccountDatum (adReviewCredit),
    AccountRedeemer (Collect, Create, Sign),
    AccountType,
    applyCAS,
    findAccountDatum,
  )
-- import Membership.Sample

-- import Membership.Sample as S
-- import Membership.Utils

import Membership.Contract
  ( ContractDatum,
    findContract,
    findContractDatum,
  )
import Membership.OnChain.Account
import Membership.OnChain.Contract
import Membership.PlatformSettings (PlatformSettings (..))
import qualified Membership.Sample as S
import Membership.Signature (findSignature, findSignatures, makeSigToken)
import Plutus.Contract as Contract
  ( Contract,
    Endpoint,
    Promise (awaitPromise),
    awaitTxConfirmed,
    endpoint,
    handleError,
    logError,
    logInfo,
    mapError,
    ownPubKey,
    submitTxConstraintsWith,
    tell,
    utxoAt,
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
import PlutusTx.Prelude
  ( AdditiveGroup ((-)),
    Bool,
    Eq ((==)),
    Integer,
    Maybe (..),
    length,
    map,
    negate,
    return,
    traceError,
    ($),
    (++),
    (.),
    (<$>),
  )
import Wallet.Emulator.Wallet ()
import Prelude (Semigroup (..), Show (..), String, uncurry)

-- Returns all accounts, their Datum, the Value of fees they hold and their owners
{-# INLINEABLE findAccounts #-}
findAccounts :: PlatformSettings -> Contract w s Text [(TxOutRef, TxOutTx, AccountDatum, Value, PubKeyHash)]
findAccounts ps = do
  utxos <- utxoAt (accountAddress ps)
  return $ map f (Map.toList utxos)
  where
    dsetTokens :: TxOutTx -> Integer
    dsetTokens o = assetClassValueOf (txOutValue $ txOutTxOut o) (psToken ps)

    sigPubKey :: Value -> PubKeyHash
    sigPubKey v = case findSignature (psSignatureSymbol ps) v of
      Just pkh -> pkh
      _ -> traceError "invalid account"

    f :: (TxOutRef, TxOutTx) -> (TxOutRef, TxOutTx, AccountDatum, Value, PubKeyHash)
    f (oref, o) = case ( findSignatures (psSignatureSymbol ps) (txOutValue $ txOutTxOut o),
                         findAccountDatum (txOutTxOut o) (lookupDatum (txOutTxTx o))
                       ) of
      ([], Just dat) ->
        ( oref,
          o,
          dat,
          txOutValue (txOutTxOut o)
            <> negate (assetClassValue (psToken ps) (dsetTokens o)),
          sigPubKey $ txOutValue (txOutTxOut o)
        )
      (_, Just dat) ->
        ( oref,
          o,
          dat,
          txOutValue (txOutTxOut o)
            <> negate (assetClassValue (psToken ps) (dsetTokens o - adReviewCredit dat)),
          sigPubKey $ txOutValue (txOutTxOut o)
        )
      _ -> traceError "invalid account"

-- Return the UTxO from the account with a PubKeyHash
{-# INLINEABLE findAccount #-}
findAccount :: PubKeyHash -> PlatformSettings -> Contract w s Text (Maybe (TxOutRef, TxOutTx))
findAccount owner ps = do
  utxos <- Map.filter f <$> utxoAt (accountAddress ps)
  return $ case Map.toList utxos of
    [(oref, o)] -> Just (oref, o)
    _ -> Nothing
  where
    f :: TxOutTx -> Bool
    f o = findSignatures (psSignatureSymbol ps) (txOutValue $ txOutTxOut o) == [owner]

-- createAccount :: forall s. PlatformSettings -> Contract () s Text ()
-- createAccount ps = do
--   pkh <- pubKeyHash <$> Contract.ownPubKey
--   let v =
--         assetClassValue (psToken ps) (psEntranceFee ps)
--           <> singleton (psSignatureSymbol ps) (makeSigToken pkh) 100
--       tx = Constraints.mustPayToOtherScript (accountValidatorHash ps) (Datum $ PlutusTx.toBuiltinData initDatum) v
--   ledgerTx <- submitTxConstraints @AccountType (typedAccountValidator ps) tx
--   logInfo @String $ show pkh
--   logInfo @String $ show (txData ledgerTx)
--   logInfo @String $ "initialised account"

createContract :: PlatformSettings -> ContractDatum -> Contract (Last AssetClass) s Text ()
createContract ps dat = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  m <- findAccount pkh ps
  case m of
    Just (oref, TxOutTx outTx o) -> do
      case findAccountDatum o (lookupDatum outTx) of
        Just aDat -> do
          osc <-
            mapError
              (pack . show)
              (mintContract pkh [("", 1)] :: Contract w s CurrencyError OneShotCurrency)
          let cs = Currency.currencySymbol osc
              vh = accountValidatorHash ps
              cv =
                singleton (psSignatureSymbol ps) (makeSigToken pkh vh) 1
                  <> Currency.mintedValue osc
              av =
                txOutValue o
                  <> negate (singleton (psSignatureSymbol ps) (makeSigToken pkh vh) 1)
                  <> assetClassValue (psToken ps) (psTxFee ps)
              lookups =
                Constraints.unspentOutputs (Map.singleton oref (TxOutTx outTx o))
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
          _ <- submitTxConstraintsWith @AccountType lookups tx
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
          let vh = accountValidatorHash ps
              cv =
                txOutValue o'
                  <> singleton (psSignatureSymbol ps) (makeSigToken pkh vh) 1
              av =
                txOutValue o
                  <> negate (singleton (psSignatureSymbol ps) (makeSigToken pkh vh) 1)
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
          _ <- submitTxConstraintsWith @ContractType lookups tx
          logInfo @String $ "signed contract"
        _ -> logError @String "account or contract datum not found"
    _ -> logError @String "account or contract not found"

collectFees :: PlatformSettings -> Contract w s Text ()
collectFees ps = do
  xs <- findAccounts ps
  case xs of
    [] -> logInfo @String "no accounts found"
    _ -> do
      let lookups =
            Constraints.unspentOutputs (Map.fromList [(oref, o) | (oref, o, _, _, _) <- xs])
              <> Constraints.otherScript (accountValidator ps)
          tx =
            M.mconcat
              [ Constraints.mustSpendScriptOutput
                  oref
                  $ Redeemer $ PlutusTx.toBuiltinData (Collect wPkh)
                | (oref, _, _, _, wPkh) <- xs
              ]
              <> M.mconcat
                [ Constraints.mustPayToOtherScript
                    (accountValidatorHash ps)
                    (Datum $ PlutusTx.toBuiltinData dat)
                    v
                  | (_, _, dat, v, _) <- xs
                ]
      ledgerTx <- submitTxConstraintsWith @AccountType lookups tx
      awaitTxConfirmed $ txId ledgerTx
      logInfo @String $ "collected from " ++ show (length xs) ++ " account(s)"

-- type AccountSchema =
--   Endpoint "start" PlatformSettings
--     .\/ Endpoint "create" (PlatformSettings, ContractDatum)
--     .\/ Endpoint "sign" (PlatformSettings, AssetClass)
--     .\/ Endpoint "collect" PlatformSettings

-- startEndpoint :: Contract () AccountSchema Text ()
-- startEndpoint = forever $
--   handleError logError $
--     awaitPromise $
--       endpoint @"start" $ \ps -> createAccount ps

type AccountSchema =
  Endpoint "create" (PlatformSettings, ContractDatum)
    .\/ Endpoint "sign" (PlatformSettings, AssetClass)
    .\/ Endpoint "collect" PlatformSettings

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

collectEndpoint :: Contract () AccountSchema Text ()
collectEndpoint = forever $
  handleError logError $
    awaitPromise $
      endpoint @"collect" $ \ps -> collectFees ps
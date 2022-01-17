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

module Account.Safe.OffChain where

import Account
import Account.Create
import Account.Safe.OnChain
import Control.Monad
import qualified Data.Map as HaskellMap
import Data.Text (Text)
import Ledger hiding (singleton)
import Ledger.Constraints as Constraints
import qualified Ledger.Constraints.TxConstraints as Constraints
import Ledger.Contexts
import Ledger.Scripts
import Ledger.Typed.Scripts
import Ledger.Value as Value
import Plutus.ChainIndex
import Plutus.ChainIndex.Tx (ChainIndexTx)
import Plutus.Contract as Contract
import qualified PlutusTx
import qualified PlutusTx.AssocMap as PlutusMap
import PlutusTx.Prelude
  ( Bool (..),
    Integer,
    Maybe (..),
    fst,
    length,
    return,
    snd,
    ($),
    (++),
    (-),
    (<$>),
    (<>),
  )
import Text.Printf (printf)
import Utils
import qualified Prelude as Haskell

-- Create account, mint's 100 SIG tokens with the user's public key hash
-- embeded on and transfers it directly to a new account UTxO, therefore
-- "creating a new account"
createAccount :: AccountSettings -> Contract () AccountSchema Text ()
createAccount cas = do
  -- The public key hash from the user who is trying to create an account
  pmtPkh <- Contract.ownPaymentPubKeyHash

  let pkh :: PubKeyHash
      pkh = unPaymentPubKeyHash pmtPkh
    
      -- The currency symbol we'll use to create the SIG token
      sigSymbol :: CurrencySymbol
      sigSymbol = signatureCurrencySymbol cas

      -- The token name, which is going to be used to build our SIG token
      sigTokenName :: TokenName
      sigTokenName = parsePubKeyHash pkh

      entranceFeeValue, sigTokensValue :: Value
      -- The value needed to pay for the platform in order to create an account
      entranceFeeValue = assetClassValue (casToken cas) (casEntranceFee cas)
      -- The sig tokens which will me minted with the user's public key hash
      -- and the account validator hash embeded
      sigTokensValue = singleton sigSymbol sigTokenName 100

      lookups :: ScriptLookups AccountType
      lookups = Constraints.mintingPolicy (signaturePolicy cas)

      tx :: TxConstraints (RedeemerType AccountType) (DatumType AccountType)
      tx =
        Constraints.mustMintValueWithRedeemer
          (Redeemer $ PlutusTx.toBuiltinData pkh)
          sigTokensValue
          Haskell.<> Constraints.mustPayToOtherScript
            accountValidatorHash
            (Datum $ PlutusTx.toBuiltinData $ initDatum sigSymbol (casTickets cas))
            (sigTokensValue <> entranceFeeValue)

  -- Submits the transaction to the blockchain
  void $ submitTxConstraintsWith @AccountType lookups tx

  Contract.logInfo
    @Haskell.String
    $ printf "%s successfully created account" (Haskell.show pkh)

-- Return the UTxO from the account with a PubKeyHash
findAccount ::
  forall w s.
  PubKeyHash ->
  AccountSettings ->
  Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, AccountDatum))
findAccount pkh sett = do
  -- All UTxOs located at the account address
  utxos <- HaskellMap.filter f <$> utxosTxOutTxAt accountAddress

  -- Return UTxO if there is only one UTxO in the filtered list
  return $ case HaskellMap.toList utxos of
    [(oref, o)] -> do
      let cTxOut :: ChainIndexTxOut
          cTxOut = Haskell.fst o

          cTxTx :: ChainIndexTx
          cTxTx = Haskell.snd o
      dat <- findAccountDatum (toTxOut cTxOut) (lookupChainIndexDatum cTxTx)
      return (oref, cTxOut, dat)
    _ -> Nothing
  where
    sigSymbol :: CurrencySymbol
    sigSymbol = signatureCurrencySymbol sett

    -- Only return UTxOs that contain a SIG values, whose signatory is the given user
    f :: (ChainIndexTxOut, ChainIndexTx) -> Haskell.Bool
    f (cTxOut, _) =
      valueOf (txOutValue $ toTxOut cTxOut) sigSymbol (parsePubKeyHash pkh)
        Haskell.> 0

displayAccount ::
  forall w s.
  PubKeyHash ->
  AccountSettings ->
  Contract w s Text ()
displayAccount pkh sett = do
  m <- findAccount pkh sett

  case m of
    Nothing -> logError @Haskell.String "No account found"
    Just (oref, o, dat) -> do
      logInfo @AccountInfo (parseAccount pkh o dat)

type AccountSchema =
  Endpoint "create-account" AccountSettings
    .\/ Endpoint "display-account" (PubKeyHash, AccountSettings)

accountEndpoints :: Contract () AccountSchema Text ()
accountEndpoints =
  forever $
    handleError logError $
      awaitPromise $
        createAccount' `select` displayAccount'
  where
    createAccount' = endpoint @"create-account" $ \sett -> createAccount sett
    displayAccount' =
      endpoint @"display-account" $ \(pkh, sett) -> displayAccount pkh sett
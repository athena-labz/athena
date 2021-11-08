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
import Control.Monad (forever, void)
import qualified Data.Map as Map
import Data.Monoid (Last (Last), mempty)
import Data.Monoid as M (Last (Last), Monoid (mconcat))
import Data.Text (Text)
import Ledger hiding (singleton)
import Ledger.Constraints as Constraints
import qualified Ledger.Constraints.TxConstraints as Constraints
import Ledger.Contexts (pubKeyHash)
import Ledger.Scripts
import Ledger.Typed.Scripts (ValidatorTypes (..))
import Ledger.Value as Value
  ( AssetClass,
    CurrencySymbol,
    TokenName,
    Value,
    assetClassValue,
    assetClassValueOf,
    singleton,
  )
import Plutus.ChainIndex.Tx (ChainIndexTx)
import Plutus.Contract as Contract
  ( Contract,
    Endpoint,
    Promise (awaitPromise),
    awaitTxConfirmed,
    endpoint,
    handleError,
    logError,
    logInfo,
    ownPubKey,
    select,
    submitTxConstraintsWith,
    tell,
    type (.\/),
  )
import qualified PlutusTx
import qualified PlutusTx.AssocMap as AM
import PlutusTx.Prelude
  ( Integer,
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
import qualified Prelude as Haskell

-- Create account, mint's 100 SIG tokens with the user's public key hash
-- embeded on and transfers it directly to a new account UTxO, therefore
-- "creating a new account"
createAccount :: CreateAccountSettings -> Contract () AccountSchema Text ()
createAccount cas = do
  -- The public key hash from the user who is trying to create an account
  pkh <- pubKeyHash <$> Contract.ownPubKey

  let -- The currency symbol we'll use to create the SIG token
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
  ledgerTx <- submitTxConstraintsWith @AccountType lookups tx

  -- Waits for the transaction to be confirmed
  void $ awaitTxConfirmed $ txId ledgerTx

  Contract.logInfo
    @Haskell.String
    $ printf "%s successfully created account" (Haskell.show pkh)

type AccountSchema =
  Endpoint "create-account" CreateAccountSettings

accountEndpoints :: Contract () AccountSchema Text ()
accountEndpoints =
  forever $
    handleError logError $
      awaitPromise $
        createAccount'
  where
    createAccount' = endpoint @"create-account" $ \cas -> createAccount cas
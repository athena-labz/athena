{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Signature where

-- Declaring data types explicitily for future reference
import Control.Monad (void)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import Data.Text (Text)
import Data.Void (Void)
import Helper (valHash)
import Ledger
  ( PubKeyHash,
    ScriptContext (scriptContextTxInfo),
    TxInInfo (txInInfoOutRef),
    TxInfo (txInfoForge, txInfoInputs),
    TxOutRef,
    ValidatorHash,
    mkMintingPolicyScript,
    ownCurrencySymbol,
    pubKeyAddress,
    pubKeyHash,
    scriptCurrencySymbol,
    txId,
    txSignedBy,
  )
import Ledger.Constraints as Constraints
  ( mintingPolicy,
    mustMintValue,
    mustSpendPubKeyOutput,
    unspentOutputs,
  )
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value
  ( CurrencySymbol,
    TokenName (TokenName),
    flattenValue,
    singleton,
  )
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract as Contract
  ( Contract,
    Endpoint,
    awaitTxConfirmed,
    endpoint,
    logError,
    logInfo,
    ownPubKey,
    submitTxConstraintsWith,
    utxoAt,
  )
import Plutus.Trace.Emulator as Emulator
  ( activateContractWallet,
    callEndpoint,
    runEmulatorTraceIO,
    waitNSlots,
  )
import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool (False),
    ByteString,
    Eq ((==)),
    any,
    traceIfFalse,
    ($),
    (&&),
    (.),
    (>>),
    (>>=),
  )
import Text.Printf (printf)
import Wallet.Emulator.Wallet (Wallet (Wallet))
import Prelude (IO, Semigroup (..), Show (..), String)

{-# INLINEABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx =
    traceIfFalse "wrong amount minted" checkMintedAmount
    && traceIfFalse "not signed by pkh" (txSignedBy info pkh)
    && traceIfFalse "minted amount must go to script" checkOutputs
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkOutputs :: Bool
    checkOutputs = (length txInfoOutputs == 2) &&
                    all f txInfoInputs
      where
        f :: TxOut -> Bool
        f (addr, val, dh) = True

    -- Check script outputs

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoForge info) of
      [(cs, tn, _)] -> (cs == ownCurrencySymbol ctx) && (tn == userToSig pkh)
      _ -> False

policy :: PubKeyHash -> Scripts.MintingPolicy
policy pkh =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||\oref' pkh' vh' -> Scripts.wrapMintingPolicy $ mkPolicy oref' pkh' vh'||])
      `PlutusTx.applyCode` PlutusTx.liftCode pkh

curSymbol :: PubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

type NFTSchema = Endpoint "mint" ValidatorHash

mint :: ValidatorHash -> Contract w NFTSchema Text ()
mint vh = do
  pk <- Contract.ownPubKey
  utxos <- utxoAt (pubKeyAddress pk)
  case Map.keys utxos of
    [] -> Contract.logError @String "no utxo found"
    oref : _ -> do
      let vhBS = B.pack . show $ vh
          tn = TokenName vhBS
          val = Value.singleton (curSymbol oref (pubKeyHash pk) vhBS) tn 1
          lookups =
            Constraints.mintingPolicy (policy oref (pubKeyHash pk) vhBS)
              <> Constraints.unspentOutputs utxos
          tx = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
      ledgerTx <- submitTxConstraintsWith @Void lookups tx
      void $ awaitTxConfirmed $ txId ledgerTx
      Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = endpoint @"mint" >>= mint

mkSchemaDefinitions ''NFTSchema

mkKnownCurrencies []

testSignature :: IO ()
testSignature = runEmulatorTraceIO $ do
  h1 <- activateContractWallet (Wallet 1) endpoints
  h2 <- activateContractWallet (Wallet 2) endpoints
  callEndpoint @"mint" h1 (valHash 1)
  callEndpoint @"mint" h2 (valHash 2)
  void $ Emulator.waitNSlots 1
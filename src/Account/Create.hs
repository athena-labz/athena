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

module Account.Create where

import Account
import Data.Aeson hiding (Value)
import GHC.Generics
import Ledger hiding (singleton)
import Ledger.Scripts
import Ledger.Typed.Scripts as Scripts
import Ledger.Value
import Plutus.ChainIndex
import qualified PlutusTx
import qualified PlutusTx.AssocMap as PlutusMap
import PlutusTx.Prelude
import qualified PlutusTx.Ratio as R
import Utils
import qualified Prelude

data CreateAccountSettings = CreateAccountSettings
  { casAccValHash :: ValidatorHash,
    casToken :: AssetClass,
    casEntranceFee :: Integer,
    casTickets :: PlutusMap.Map TokenName CurrencySymbol
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq CreateAccountSettings where
  {-# INLINEABLE (==) #-}
  (CreateAccountSettings vh tn entFee tkt)
    == (CreateAccountSettings vh' tn' entFee' tkt') =
      vh == vh' && tn == tn' && entFee == entFee' && tkt == tkt'

PlutusTx.unstableMakeIsData ''CreateAccountSettings
PlutusTx.makeLift ''CreateAccountSettings

{-# INLINEABLE parseTokenName #-}
parseTokenName :: TokenName -> PubKeyHash
parseTokenName = PubKeyHash . unTokenName

{-# INLINEABLE parsePubKeyHash #-}
parsePubKeyHash :: PubKeyHash -> TokenName
parsePubKeyHash = TokenName . getPubKeyHash

{-# INLINEABLE sigPolicyTraceIfFalse #-}
sigPolicyTraceIfFalse :: BuiltinString -> Bool -> Bool
sigPolicyTraceIfFalse msg = traceIfFalse ("Sig Policy - " <> msg)

{-# INLINEABLE sigPolicyTraceError #-}
sigPolicyTraceError :: forall a. BuiltinString -> a
sigPolicyTraceError msg = traceError ("Sig Policy - " <> msg)

-- The policy that will allow or not the minting of new signature tokens
-- Signature tokens are DigiService's way of authenticating accounts,
-- making sure the fees are paid and identifying users
{-# INLINEABLE mkSignaturePolicy #-}
mkSignaturePolicy :: CreateAccountSettings -> PubKeyHash -> ScriptContext -> Bool
mkSignaturePolicy sett pkh ctx =
  sigPolicyTraceIfFalse "Transaction not signed by public key" (txSignedBy info pkh)
    && sigPolicyTraceIfFalse "Invalid token name or amount" validMinting
    && sigPolicyTraceIfFalse "Invalid account value" validAccountValue
    && sigPolicyTraceIfFalse "Invalid account datum" validAccountDatum
  where
    -- The basic information about this transaction
    info :: TxInfo
    info = scriptContextTxInfo ctx

    expectedTokenName :: TokenName
    expectedTokenName = parsePubKeyHash pkh

    validMinting :: Bool
    validMinting = case flattenValue (txInfoMint info) of
      [(_, tn, amt)] -> tn == expectedTokenName && amt == 100
      _ -> False

    sigValue :: Value
    sigValue = singleton (ownCurrencySymbol ctx) expectedTokenName 100

    -- The entrance fee required to enter in the platform
    fees :: Value
    fees = assetClassValue (casToken sett) (casEntranceFee sett)

    accountOutput :: TxOut
    accountOutput = case strictFindOutputWithValHash (casAccValHash sett) info of
      Just o -> o
      Nothing -> sigPolicyTraceError "Unique account output missing"

    accountDatum :: AccountDatum
    accountDatum = case findAccountDatum accountOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> sigPolicyTraceError "Datum not found"

    validAccountValue :: Bool
    validAccountValue = txOutValue accountOutput == sigValue <> fees

    validAccountDatum :: Bool
    validAccountDatum =
      accountDatum == initDatum (ownCurrencySymbol ctx) (casTickets sett)

signaturePolicy :: CreateAccountSettings -> Scripts.MintingPolicy
signaturePolicy sett =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkSignaturePolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode sett

signatureCurrencySymbol :: CreateAccountSettings -> CurrencySymbol
signatureCurrencySymbol = scriptCurrencySymbol . signaturePolicy

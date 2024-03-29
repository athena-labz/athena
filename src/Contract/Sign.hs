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

module Contract.Sign where

import Account
import Contract
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

{-# INLINEABLE ticketName #-}
ticketName :: BuiltinByteString
ticketName = "sign-contract"

{-# INLINEABLE mkSignContractPolicy #-}
mkSignContractPolicy ::
  ContractSettings ->
  (PubKeyHash, Integer, AssetClass) ->
  ScriptContext ->
  Bool
mkSignContractPolicy sett (pkh, role, nft) ctx =
  traceIfFalse "Sign Contract - Invalid ticket produced" (validTicket ctx "sign-contract" 2)
    && traceIfFalse "Sign Contract - User not allowed" userAllowed
    && traceIfFalse "Sign Contract - Missing signature" (txSignedBy info pkh)
    && traceIfFalse "Sign Contract - Invalid account datum" validAccountDatum
    && traceIfFalse "Sign Contract - Invalid account value" validAccountValue
    && traceIfFalse "Sign Contract - Invalid contract datum" validContractDatum
    && traceIfFalse "Sign Contract - Invalid contract value" validContractValue
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    accountInput :: TxOut
    accountInput = case strictFindInputWithValHash (ccsAccValHash sett) info of
      Just o -> o
      Nothing -> traceError "Create Contract - Account input not found"

    accountOutput :: TxOut
    accountOutput = case strictFindOutputWithValHash (ccsAccValHash sett) info of
      Just o -> o
      Nothing -> traceError "Create Contract - Account input not found"

    inputAccountDatum :: AccountDatum
    inputAccountDatum = case findAccountDatum accountInput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Create Contract - Account input datum not found"

    outputAccountDatum :: AccountDatum
    outputAccountDatum = case findAccountDatum accountOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Create Contract - Account output datum not found"

    contractInput :: TxOut
    contractInput = case strictFindInputWithValHash (ccsCtrValHash sett) info of
      Just o -> o
      Nothing -> traceError "Sign Contract - Contract input not found"

    contractOutput :: TxOut
    contractOutput = case strictFindOutputWithValHash (ccsCtrValHash sett) info of
      Just o -> o
      Nothing -> traceError "Sign Contract - Contract output not found"

    inputContractDatum :: ContractDatum
    inputContractDatum = case findContractDatum contractInput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Sign Contract - Contract input datum not found"

    outputContractDatum :: ContractDatum
    outputContractDatum = case findContractDatum contractOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Sign Contract - Contract output datum not found"

    sig :: AssetClass
    sig = assetClass (cdSigSymbol inputContractDatum) (parsePubKeyHash pkh)

    userAllowed :: Bool
    userAllowed = case cdPrivacyType inputContractDatum of
      PT_Public -> True
      PT_Private -> isJust (PlutusMap.lookup pkh (cdRoleMap inputContractDatum))

    validAccountDatum :: Bool
    validAccountDatum =
      addContractToAccount inputAccountDatum nft == outputAccountDatum

    validAccountValue :: Bool
    validAccountValue =
      txOutValue accountOutput
        == ( txOutValue accountInput
               <> negate (assetClassValue sig 1)
               <> singleton (ownCurrencySymbol ctx) (TokenName ticketName) 1
           )

    validContractDatum :: Bool
    validContractDatum = case cdPrivacyType inputContractDatum of
      PT_Public ->
        addUserToContract pkh role inputContractDatum == outputContractDatum
      PT_Private -> inputContractDatum == outputContractDatum

    validContractValue :: Bool
    validContractValue =
      txOutValue contractOutput
        == ( txOutValue contractInput
               <> assetClassValue sig 1
               <> (cdCollateral inputContractDatum)
               <> singleton (ownCurrencySymbol ctx) (TokenName ticketName) 1
           )

signContractPolicy :: ContractSettings -> Scripts.MintingPolicy
signContractPolicy sett =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkSignContractPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode sett

signContractCurrencySymbol :: ContractSettings -> CurrencySymbol
signContractCurrencySymbol = scriptCurrencySymbol . signContractPolicy

signContractPlutusScript :: ContractSettings -> Script
signContractPlutusScript = unMintingPolicyScript . signContractPolicy

signContractValidator :: ContractSettings -> Validator
signContractValidator = Validator . signContractPlutusScript


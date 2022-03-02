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

module Contract.Create where

import Account
import Contract
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

{-# INLINABLE ticketName #-}
ticketName :: BuiltinByteString
ticketName = "create-contract"

{-# INLINEABLE mkCreateContractPolicy #-}
mkCreateContractPolicy ::
  ContractSettings ->
  (PubKeyHash, Integer, AssetClass) ->
  ScriptContext ->
  Bool
mkCreateContractPolicy sett (pkh, role, nft) ctx =
  traceIfFalse "Create Contract - Invalid ticket produced" (validTicket ctx "create-contract" 1)
    && traceIfFalse "Create Contract - Missing signature" (txSignedBy info pkh)
    && traceIfFalse "Create Contract - Invalid account datum" validAccountDatum
    && traceIfFalse "Create Contract - Invalid account value" validAccountValue
    && traceIfFalse "Create Contract - Invalid contract datum" validContractDatum
    && traceIfFalse "Create Contract - Invalid contract value" validContractValue
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

    contractOutput :: TxOut
    contractOutput = case strictFindOutputWithValHash (ccsCtrValHash sett) info of
      Just o -> o
      Nothing -> traceError "Create Contract - Contract output not found"

    contractDatum :: ContractDatum
    contractDatum = case findContractDatum contractOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Create Contract - Contract output datum not found"

    sig :: AssetClass
    sig = assetClass (adSigSymbol inputAccountDatum) (parsePubKeyHash pkh)

    collateral :: Value
    collateral = case PlutusMap.lookup role (cdRoles contractDatum) of
      Just val -> val
      Nothing -> traceError "Create Contract - Role does not exist"

    validAccountDatum :: Bool
    validAccountDatum =
      outputAccountDatum == (addContractToAccount inputAccountDatum nft)

    validAccountValue :: Bool
    validAccountValue =
      txOutValue accountOutput
        == ( txOutValue accountInput
               <> negate (assetClassValue sig 1)
               <> singleton (ownCurrencySymbol ctx) (TokenName ticketName) 1
           )

    validContractDatum :: Bool
    validContractDatum =
      (cdSigSymbol contractDatum == adSigSymbol inputAccountDatum)
        && (cdPublisher contractDatum == pkh)
        && (validRoles contractDatum)
        && (null $ cdAccusations contractDatum)
        && ( case cdPrivacyType contractDatum of
               PT_Private -> pkh `elem` roleMapKeys
               PT_Public -> roleMapKeys == [pkh]
           )
      where
        roleMapKeys :: [PubKeyHash]
        roleMapKeys = PlutusMap.keys (cdRoleMap contractDatum)

    validContractValue :: Bool
    validContractValue =
      txOutValue contractOutput
        == ( assetClassValue sig 1
               <> assetClassValue nft 1
               <> collateral
           )

createContractPolicy :: ContractSettings -> Scripts.MintingPolicy
createContractPolicy sett =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkCreateContractPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode sett

createContractCurrencySymbol :: ContractSettings -> CurrencySymbol
createContractCurrencySymbol = scriptCurrencySymbol . createContractPolicy

createContractPlutusScript :: ContractSettings -> Script
createContractPlutusScript = unMintingPolicyScript . createContractPolicy

createContractValidator :: ContractSettings -> Validator
createContractValidator = Validator . createContractPlutusScript

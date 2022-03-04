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

module Contract.Quit where

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

{-# INLINEABLE mkQuitContractPolicy #-}
mkQuitContractPolicy ::
  ContractSettings ->
  (PubKeyHash, AssetClass) ->
  ScriptContext ->
  Bool
mkQuitContractPolicy sett (pkh, nft) ctx =
  traceIfFalse "Invalid ticket produced" (validTicket ctx "quit-contract" 2)
    && traceIfFalse "User under accusation" userAllowed
    && traceIfFalse "Missing signature" (txSignedBy info pkh)
    && traceIfFalse "Invalid account datum" validAccountDatum
    && traceIfFalse "Invalid account value" validAccountValue
    && traceIfFalse "Invalid contract datum" validContractDatum
    && traceIfFalse "Invalid contract value" validContractValue
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    accountInput :: TxOut
    accountInput = case strictFindInputWithValHash (ccsAccValHash sett) info of
      Just o -> o

    accountOutput :: TxOut
    accountOutput = case strictFindOutputWithValHash (ccsAccValHash sett) info of
      Just o -> o

    inputAccountDatum :: AccountDatum
    inputAccountDatum = case findAccountDatum accountInput (`findDatum` info) of
      Just dat -> dat

    outputAccountDatum :: AccountDatum
    outputAccountDatum = case findAccountDatum accountOutput (`findDatum` info) of
      Just dat -> dat

    contractInput :: TxOut
    contractInput = case strictFindInputWithValHash (ccsCtrValHash sett) info of
      Just o -> o

    contractOutput :: TxOut
    contractOutput = case strictFindOutputWithValHash (ccsCtrValHash sett) info of
      Just o -> o

    inputContractDatum :: ContractDatum
    inputContractDatum = case findContractDatum contractInput (`findDatum` info) of
      Just dat -> dat

    outputContractDatum :: ContractDatum
    outputContractDatum = case findContractDatum contractOutput (`findDatum` info) of
      Just dat -> dat

    sig :: AssetClass
    sig = assetClass (cdSigSymbol inputContractDatum) (parsePubKeyHash pkh)

    role :: Integer
    role = case PlutusMap.lookup pkh (cdRoleMap inputContractDatum) of
      Just (r, _) -> r

    collateral :: Value
    collateral = case PlutusMap.lookup role (cdRoles inputContractDatum) of
      Just val -> val

    percentage :: Integer
    percentage = case PlutusMap.lookup pkh (cdRoleMap inputContractDatum) of
      Just (_, p) -> p

    consumed :: Value
    consumed = percentageFromValue percentage collateral

    ticketVal :: Value
    ticketVal = singleton (ownCurrencySymbol ctx) (TokenName "quit-contract") 1

    userAllowed :: Bool
    userAllowed =
      all (\acc -> aAccused acc /= pkh) (cdAccusations inputContractDatum)
        && all (\(acc, _) -> aAccused acc /= pkh) (cdResolutions inputContractDatum)

    validAccountDatum :: Bool
    validAccountDatum =
      outputAccountDatum == removeContractFromAccount inputAccountDatum nft

    validAccountValue :: Bool
    validAccountValue =
      txOutValue accountOutput
        == ( txOutValue accountInput
               <> assetClassValue sig 1
               <> ticketVal
           )

    validContractDatum :: Bool
    validContractDatum =
      outputContractDatum == removeUserFromContract pkh inputContractDatum

    validContractValue :: Bool
    validContractValue =
      txOutValue contractOutput
        == ( txOutValue contractInput
               <> negate (assetClassValue sig 1)
               <> negate (assetClassValue nft 1)
               <> negate consumed
               <> ticketVal
           )

quitContractPolicy :: ContractSettings -> Scripts.MintingPolicy
quitContractPolicy sett =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkQuitContractPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode sett

quitContractCurrencySymbol :: ContractSettings -> CurrencySymbol
quitContractCurrencySymbol = scriptCurrencySymbol . quitContractPolicy

quitContractPlutusScript :: ContractSettings -> Script
quitContractPlutusScript = unMintingPolicyScript . quitContractPolicy

quitContractValidator :: ContractSettings -> Validator
quitContractValidator = Validator . quitContractPlutusScript


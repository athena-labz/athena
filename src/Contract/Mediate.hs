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

module Contract.Mediate where

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
ticketName = "resolve-dispute"

{-# INLINEABLE mkResolveDisputePolicy #-}
mkResolveDisputePolicy ::
  ContractSettings ->
  (PubKeyHash, BuiltinByteString, POSIXTime, POSIXTime) ->
  ScriptContext ->
  Bool
mkResolveDisputePolicy sett (pkh, vdt, time, dln) ctx =
  traceIfFalse "Invalid ticket produced" (validTicket ctx ticketName 1)
    && traceIfFalse "User not allowed" userAllowed
    && traceIfFalse "Missing signature" (txSignedBy info pkh)
    && traceIfFalse "Invalid time" (time `member` (txInfoValidRange info))
    && traceIfFalse "Invalid deadline" validDeadline
    && traceIfFalse "Deadline passed" (time `member` (to dln))
    && traceIfFalse "Invalid contract datum" validContractDatum
    && traceIfFalse "Invalid contract value" validContractValue
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

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

    -- TODO: In the future scripts should also be allowed to be judges
    userAllowed :: Bool
    userAllowed =
      currentJudge inputContractDatum
        == Just (pubKeyHashAddress (PaymentPubKeyHash pkh) Nothing)

    validDeadline :: Bool
    validDeadline = dln == aDeadline (last (cdAccusations inputContractDatum))

    validContractDatum :: Bool
    validContractDatum =
      outputContractDatum == resolveDisputeInContract vdt inputContractDatum

    validContractValue :: Bool
    validContractValue =
      txOutValue contractOutput
        == ( txOutValue contractInput
               <> singleton (ownCurrencySymbol ctx) (TokenName ticketName) 1
           )

resolveDisputePolicy :: ContractSettings -> Scripts.MintingPolicy
resolveDisputePolicy sett =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkResolveDisputePolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode sett

resolveDisputeCurrencySymbol :: ContractSettings -> CurrencySymbol
resolveDisputeCurrencySymbol = scriptCurrencySymbol . resolveDisputePolicy

resolveDisputePlutusScript :: ContractSettings -> Script
resolveDisputePlutusScript = unMintingPolicyScript . resolveDisputePolicy

resolveDisputeValidator :: ContractSettings -> Validator
resolveDisputeValidator = Validator . resolveDisputePlutusScript

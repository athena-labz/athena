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
  (PubKeyHash, BuiltinByteString, POSIXTime) ->
  ScriptContext ->
  Bool
mkResolveDisputePolicy sett (pkh, vdt, dln) ctx =
  traceIfFalse "Resolve Dispute - Invalid ticket produced" (validTicket ctx ticketName 1)
    && traceIfFalse "Resolve Dispute - User not allowed" userAllowed
    && traceIfFalse "Resolve Dispute - Missing signature" (txSignedBy info pkh)
    && traceIfFalse "Resolve Dispute - Invalid deadline" validDeadline
    && traceIfFalse "Resolve Dispute - Deadline passed" ((to dln) `contains` (txInfoValidRange info))
    && traceIfFalse "Resolve Dispute - Invalid contract datum" validContractDatum
    && traceIfFalse "Resolve Dispute - Invalid contract value" validContractValue
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    contractInput :: TxOut
    contractInput = case strictFindInputWithValHash (ccsCtrValHash sett) info of
      Just o -> o
      Nothing -> traceError "Resolve Dispute - Contract input not found"

    contractOutput :: TxOut
    contractOutput = case strictFindOutputWithValHash (ccsCtrValHash sett) info of
      Just o -> o
      Nothing -> traceError "Resolve Dispute - Contract output not found"

    inputContractDatum :: ContractDatum
    inputContractDatum = case findContractDatum contractInput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Resolve Dispute - Contract input datum not found"

    outputContractDatum :: ContractDatum
    outputContractDatum = case findContractDatum contractOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "Resolve Dispute - Contract output datum not found"

    -- TODO: In the future scripts should also be allowed to be judges
    userAllowed :: Bool
    userAllowed = currentJudge inputContractDatum == Just (pubKeyHashAddress pkh)

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

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

module Contract.Accuse where

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

{-# INLINEABLE ticketName #-}
ticketName :: BuiltinByteString
ticketName = "raise-dispute"

{-# INLINEABLE mkRaiseDisputePolicy #-}
mkRaiseDisputePolicy ::
  ContractSettings ->
  (PubKeyHash, PubKeyHash, POSIXTime, POSIXTime) ->
  ScriptContext ->
  Bool
mkRaiseDisputePolicy sett (pkh, acd, time, dln) ctx =
  traceIfFalse "Invalid ticket produced" (validTicket ctx ticketName 1)
    && traceIfFalse "User not allowed" userAllowed
    && traceIfFalse "Missing signature" (txSignedBy info pkh)
    && traceIfFalse "Invalid time" (time `member` (txInfoValidRange info))
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

    userAllowed :: Bool
    userAllowed =
      isJust (PlutusMap.lookup pkh (cdRoleMap inputContractDatum))
        && ( case cdRelationType inputContractDatum of
               RT_Convergent ->
                 ((cdPublisher inputContractDatum) == pkh)
                   || ((cdPublisher inputContractDatum) == acd)
               RT_Distributed -> True
           )

    validContractDatum :: Bool
    validContractDatum =
      outputContractDatum
        == addAccusationToContract (Accusation pkh acd time dln) inputContractDatum

    validContractValue :: Bool
    validContractValue =
      txOutValue contractOutput
        == ( txOutValue contractInput
               <> singleton (ownCurrencySymbol ctx) (TokenName ticketName) 1
           )

raiseDisputePolicy :: ContractSettings -> Scripts.MintingPolicy
raiseDisputePolicy sett =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkRaiseDisputePolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode sett

raiseDisputeCurrencySymbol :: ContractSettings -> CurrencySymbol
raiseDisputeCurrencySymbol = scriptCurrencySymbol . raiseDisputePolicy

raiseDisputePlutusScript :: ContractSettings -> Script
raiseDisputePlutusScript = unMintingPolicyScript . raiseDisputePolicy

raiseDisputeValidator :: ContractSettings -> Validator
raiseDisputeValidator = Validator . raiseDisputePlutusScript


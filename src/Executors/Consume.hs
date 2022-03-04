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

module Executors.Consume where

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
import qualified PlutusTx.Ratio as R

-- Empty string means party is innocent
{-# INLINEABLE mkConsumeCollateralPolicy #-}
mkConsumeCollateralPolicy ::
  ContractSettings ->
  (Integer, Integer) ->
  ScriptContext ->
  Bool
mkConsumeCollateralPolicy sett (perc, idx) ctx =
  traceIfFalse "Invalid ticket produced" (validTicket ctx "consume-collateral" 1)
    && traceIfFalse "Invalid percentage" validPercentage
    && traceIfFalse "Invalid contract datum" validContractDatum
    && traceIfFalse "Invalid contract value" validContractValue
    && traceIfFalse "Not signed by accuser" (txSignedBy info accuser)
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

    resolution :: (Accusation, BuiltinByteString)
    resolution = (cdResolutions inputContractDatum) !! idx

    accuser :: PubKeyHash
    accuser = aAccuser $ fst resolution

    accused :: PubKeyHash
    accused = aAccused $ fst resolution

    role :: Integer
    role = case PlutusMap.lookup accused (cdRoleMap inputContractDatum) of
      Just (r, p)
        | p - perc < 0 -> traceError "Unbounded percentage"
        | otherwise -> r

    collateral :: Value
    collateral = case PlutusMap.lookup role (cdRoles inputContractDatum) of
      Just val -> val

    consumed :: Value
    consumed = percentageFromValue perc collateral

    validPercentage :: Bool
    validPercentage = intToBuiltinByteString perc == snd resolution

    validContractDatum :: Bool
    validContractDatum =
      outputContractDatum
        == subtractFromCollateral perc accused (removeResolutionFromContract idx inputContractDatum)

    validContractValue :: Bool
    validContractValue =
      txOutValue contractOutput
        == ( txOutValue contractInput
               <> negate consumed
               <> singleton (ownCurrencySymbol ctx) (TokenName "consume-collateral") 1
           )


consumeCollateralPolicy :: ContractSettings -> Scripts.MintingPolicy
consumeCollateralPolicy sett =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkConsumeCollateralPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode sett

consumeCollateralCurrencySymbol :: ContractSettings -> CurrencySymbol
consumeCollateralCurrencySymbol = scriptCurrencySymbol . consumeCollateralPolicy

consumeCollateralPlutusScript :: ContractSettings -> Script
consumeCollateralPlutusScript = unMintingPolicyScript . consumeCollateralPolicy

consumeCollateralValidator :: ContractSettings -> Validator
consumeCollateralValidator = Validator . consumeCollateralPlutusScript

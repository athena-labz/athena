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

module Test.Sample where

import Account
import Account.Create
import Account.Safe.OnChain
import Contract
import Contract.Create
import Contract.Safe.OnChain
import Ledger
import Ledger.Ada
import Ledger.Value
import qualified PlutusTx.AssocMap as PlutusMap
import PlutusTx.Prelude

adaAssetClass :: AssetClass
adaAssetClass = assetClass adaSymbol adaToken

sampleAccountSettings :: [AssetClass] -> AccountSettings
sampleAccountSettings tkts =
  AccountSettings
    { casAccValHash = accountValidatorHash,
      casToken = adaAssetClass,
      casEntranceFee = 5_000_000,
      casTickets = tkts
    }

sampleContractSettings :: ContractSettings
sampleContractSettings =
  ContractSettings
    { ccsAccValHash = accountValidatorHash,
      ccsCtrValHash = contractValidatorHash,
      ccsToken = adaAssetClass
    }

sampleContractDatum ::
  PubKeyHash ->
  Integer ->
  [Address] ->
  [AssetClass] ->
  ContractDatum
sampleContractDatum pkh amt jdgs tkts =
  ContractDatum
    { cdRelationType = RT_Distributed,
      cdPrivacyType = PT_Public,
      cdPublisher = pkh,
      cdCollateral = lovelaceValueOf amt,
      cdTerms = terms,
      cdJudges = jdgs,
      cdAccusations = [],
      cdRoles = 0,
      cdRoleMap = PlutusMap.fromList [(pkh, 0)],
      cdTickets = tkts
    }
  where
    input_1 :: IntegerInput
    input_1 =
      IntegerInput
        { iiRoles = [0],
          iiQuestion = "Number of books borrowed by the accused in the month"
        }

    input_2 :: IntegerInput
    input_2 =
      IntegerInput
        { iiRoles = [0],
          iiQuestion = "Number of books lent by the accused in the month"
        }

    input_3 :: BoolInput
    input_3 =
      BoolInput
        { biRoles = [0],
          biQuestion = "All books borrowed by the accused 30 (or more) days ago were returned"
        }

    input_4 :: BoolInput
    input_4 =
      BoolInput
        { biRoles = [0],
          biQuestion = "Accused was previously suspended from the club already"
        }

    -- If the number of books lent is less than the number of books borrowed...
    trigger_1 :: Trigger
    trigger_1 = TrigLessThanInp input_2 input_1

    -- If not all books borrowed 30 days ago were returned...
    trigger_2 :: Trigger
    trigger_2 = TrigEqualsBool input_3 False

    -- If the accused was already suspended from the club...
    trigger_3 :: Trigger
    trigger_3 = TrigEqualsBool input_4 True

    action_1 :: Action
    action_1 = DoInRealLife "Accused must return all borrowed books"

    action_2 :: BuiltinByteString -> Action
    action_2 time =
      DoInRealLife $
        "Accused must be suspended from the club for " `appendByteString` time

    action_3 :: Action
    action_3 = DoInRealLife "Accused must be expelled from the club"

    terms :: PlutusMap.Map Trigger [Action]
    terms =
      PlutusMap.fromList
        [ (trigger_2, [action_1, action_2 "two weeks"]), -- The worst punishment must come first
          (trigger_1, [action_1, action_2 "one week"]),
          (TrigAnd (TrigOr trigger_1 trigger_2) trigger_3, [action_1, action_3])
        ]
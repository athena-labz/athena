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

sampleAccountSettings :: [CurrencySymbol] -> AccountSettings
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

sampleContractCore ::
  PubKeyHash ->
  Integer ->
  [Address] ->
  [CurrencySymbol] ->
  ContractCore
sampleContractCore pkh amt jdgs tkts =
  ContractCore
    { ccRelationType = RT_Distributed,
      ccPrivacyType = PT_Public,
      ccTermsHash = "terms-hash",
      ccJudges = jdgs,
      ccRoles = PlutusMap.fromList [(0, lovelaceValueOf amt)],
      ccRoleMap = PlutusMap.fromList [(pkh, 0)],
      ccTickets = tkts
    }

{-# INLINABLE sampleResolution #-}
sampleResolution :: BuiltinByteString
sampleResolution = "100"
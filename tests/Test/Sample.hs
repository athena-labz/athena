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

import Account.Safe.OnChain
import Account.Create
import Ledger.Ada
import Ledger.Value
import qualified PlutusTx.AssocMap as PlutusMap

adaAssetClass :: AssetClass
adaAssetClass = assetClass adaSymbol adaToken

sampleCreateAccountSettings :: CreateAccountSettings
sampleCreateAccountSettings =
  CreateAccountSettings
    { casAccValHash = accountValidatorHash,
      casToken = adaAssetClass,
      casEntranceFee = 5_000_000,
      casTickets = PlutusMap.empty
    }
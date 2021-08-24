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

module Membership.Sample
  ( sampleToken,
    sampleSigSymbol,
    sampleEntranceFee,
    sampleTxFee,
    sampleContractVal,
    sampleContractHash,
    samplePlatformSettings,
    sampleService,
    sampleContract,
    sampleFailingContract,
    validator,
    typedValidator,
    sampleTypedValidator,
    SampleContractType
  )
where

import Ledger (Validator, ValidatorHash, mkValidatorScript, ScriptContext)
import Ledger.Crypto (pubKeyHash)
import Ledger.Value
  ( AssetClass (AssetClass),
    CurrencySymbol (CurrencySymbol),
  )
import Membership.Contract
import Membership.PlatformSettings (PlatformSettings (..))
import Membership.Service (Service (..), ServiceType (CConstant))
import qualified PlutusTx
import PlutusTx.Prelude (Bool(..), BuiltinData, Integer, Maybe (..), ($), (.))
import Wallet.Emulator.Wallet (Wallet (Wallet), walletPubKey)
import qualified Ledger.Typed.Scripts      as Scripts
import Ledger.Scripts

{-# INLINEABLE sampleToken #-}
sampleToken :: AssetClass
sampleToken = AssetClass ("aa", "DSET")

{-# INLINEABLE sampleSigSymbol #-}
sampleSigSymbol :: CurrencySymbol
sampleSigSymbol = CurrencySymbol "11"

{-# INLINEABLE sampleEntranceFee #-}
sampleEntranceFee :: Integer
sampleEntranceFee = 300_000

{-# INLINEABLE sampleTxFee #-}
sampleTxFee :: Integer
sampleTxFee = 1_000

{-# INLINEABLE mkValidator #-}
mkValidator :: Integer -> ContractDatum -> () -> ScriptContext -> Bool
mkValidator _ _ _ _ = True

data SampleContractType
instance Scripts.ValidatorTypes SampleContractType where
    type instance DatumType SampleContractType = ContractDatum
    type instance RedeemerType SampleContractType = ()

typedValidator :: Integer -> Scripts.TypedValidator SampleContractType
typedValidator i = Scripts.mkTypedValidator @SampleContractType
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode i)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @ContractDatum @()

validator :: Integer -> Validator
validator = Scripts.validatorScript . typedValidator

valHash :: Integer -> Ledger.ValidatorHash
valHash = validatorHash . validator

sampleTypedValidator :: Scripts.TypedValidator SampleContractType
sampleTypedValidator = typedValidator 694

sampleContractVal :: Validator
sampleContractVal = validator 694

sampleContractHash :: ValidatorHash
sampleContractHash = valHash 694

samplePlatformSettings :: PlatformSettings
samplePlatformSettings =
  PlatformSettings
    { psVersion = 1,
      psToken = sampleToken,
      psSignatureSymbol = sampleSigSymbol,
      psContractVH = sampleContractHash,
      psEntranceFee = sampleEntranceFee,
      psTxFee = 1_000,
      psCollectors = [pubKeyHash $ walletPubKey (Wallet 1)],
      psOldVH = Nothing
    }

{-# INLINEABLE sampleService #-}
sampleService :: Service
sampleService =
  Service
    { sPublisher = pubKeyHash $ walletPubKey $ Wallet 1,
      sTitle = "Title",
      sDescription = "Description",
      sTrust = 30,
      sType = CConstant
    }

{-# INLINEABLE sampleContract #-}
sampleContract :: ContractDatum
sampleContract =
  ContractDatum
    { cdJudges = [pubKeyHash $ walletPubKey $ Wallet w | w <- [4 .. 10]],
      cdInputs =
        [ "Was a book actually written and delivered?",
          "Did it have more than 200 pages?",
          "Was the client collaborative, providing any information needed?"
        ],
      cdLogicScript = sampleContractHash,
      cdAccusations = [],
      cdService = sampleService
    }

{-# INLINEABLE sampleFailingContract #-}
sampleFailingContract :: ContractDatum
sampleFailingContract =
  ContractDatum
    { cdJudges = [],
      cdInputs = [],
      cdLogicScript = sampleContractHash,
      cdAccusations = [],
      cdService = sampleService
    }
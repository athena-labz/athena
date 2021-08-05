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

module Helper where

import GHC.Integer (timesInteger)
import Ledger (Validator, ValidatorHash, mkValidatorScript)
import qualified Ledger.Scripts as Scripts
import qualified PlutusTx
import PlutusTx.Prelude (BuiltinData, Integer, (.), sum, foldr)
import Prelude (div)

{-# INLINEABLE mkValidator #-}
mkValidator :: Integer -> BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ _ = ()

validator :: Integer -> Validator
validator x =
  mkValidatorScript
    ($$(PlutusTx.compile [||mkValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode x)

valHash :: Integer -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . validator



type CAS = Integer

-- The total amount of tokens that will be minted every month
totalAmt :: Integer
totalAmt = 1000

calculateReward :: CAS -> [CAS] -> Integer
calculateReward score xs = (score `timesInteger` totalAmt) `div` sum xs

calculateRewards :: [CAS] -> [Integer]
calculateRewards xs = foldr (\x acc -> calculateReward x xs : acc) [] xs

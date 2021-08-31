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

module Membership.OnChain.Utils where

import Ledger
  ( PubKeyHash,
    ScriptContext,
    TxInInfo (txInInfoResolved),
    TxOut (TxOut, txOutValue),
    findOwnInput,
    getContinuingOutputs,
  )
import Ledger.Value
  ( AssetClass,
    CurrencySymbol,
    assetClassValueOf,
  )
import Membership.PlatformSettings (PlatformSettings (psToken))
import Membership.Signature (findSignatories)
import PlutusTx.Prelude
  ( AdditiveGroup ((-)),
    Bool,
    Eq ((==)),
    Integer,
    Maybe (Just, Nothing),
    Ord ((>)),
    filter,
    fst,
    snd,
    traceError,
    ($),
    (&&),
  )

-- Find output and input from context as long as there is only one script output
{-# INLINEABLE strictFindOutAndIn #-}
strictFindOutAndIn :: ScriptContext -> (TxOut, TxOut)
strictFindOutAndIn ctx = (ownInput, ownOutput)
  where
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "account input missing"
      Just i -> txInInfoResolved i

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> o
      _ -> traceError "expected exactly one output"

{-# INLINEABLE findOutAndIn #-}
findOutAndIn :: PubKeyHash -> CurrencySymbol -> ScriptContext -> (TxOut, TxOut)
findOutAndIn pkh sig ctx = (ownInput, ownOutput)
  where
    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "input missing"
      Just i -> txInInfoResolved i

    f :: TxOut -> Bool
    f (TxOut _ val _) = findSignatories sig val == [pkh]

    ownOutput :: TxOut
    ownOutput = case filter f (getContinuingOutputs ctx) of
      [o] -> o
      _ -> traceError "expected exactly one account output"

{-# INLINEABLE sigTokenIn #-}
sigTokenIn :: AssetClass -> ScriptContext -> Bool
sigTokenIn sig ctx = inputHasToken && outputHasToken
  where
    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    inputTokens :: Integer
    inputTokens = assetClassValueOf (txOutValue ownInput) sig

    inputHasToken :: Bool
    inputHasToken = inputTokens > 0

    outputTokens :: Integer
    outputTokens = assetClassValueOf (txOutValue ownOutput) sig

    outputHasToken :: Bool
    outputHasToken = ((inputTokens - outputTokens) == 1) && (outputTokens > 0)

{-# INLINEABLE sigTokenIn' #-}
sigTokenIn' :: PlatformSettings -> ScriptContext -> Bool
sigTokenIn' ps = sigTokenIn (psToken ps)
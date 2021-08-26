{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Membership.OnChain.Signature where

import Ledger.Address (Address (Address))
import Ledger.Contexts
  ( ScriptContext (scriptContextTxInfo),
    TxInfo (txInfoMint, txInfoOutputs),
    TxOut (..),
    findDatum,
    ownCurrencySymbol,
    scriptCurrencySymbol,
    txSignedBy,
  )
import Ledger.Credential (Credential (ScriptCredential))
import Ledger.Crypto (PubKeyHash)
import Ledger.Scripts
  ( Datum (Datum),
    DatumHash,
    ValidatorHash,
    mkMintingPolicyScript,
  )
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value
  ( CurrencySymbol,
    TokenName,
    Value,
    flattenValue,
    geq,
    singleton,
  )
import Membership.Account (AccountDatum, initDatum)
import Membership.Signature
  ( Sig (sScript, sTokenName, sUser),
    makeSig,
  )
import qualified PlutusTx
import PlutusTx.Prelude
  ( Bool,
    Eq ((==)),
    Integer,
    Maybe (Just),
    foldr,
    traceError,
    traceIfFalse,
    ($),
    (&&),
    (.),
    (<>),
    (==),
  )
import qualified PlutusTx.Prelude as Ptx

{-# INLINEABLE mkPolicy #-}
mkPolicy :: Value -> () -> ScriptContext -> Bool
mkPolicy fees () ctx =
  traceIfFalse "not signed by pkh" (txSignedBy info pkh)
    && traceIfFalse "wrong script output" checkOutputs
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    policySig :: Sig
    policySig = case flattenValue (txInfoMint info) of
      [(_, tn', _)] -> makeSig tn'
      _ -> Ptx.traceError "must mint one token only"

    mintedValue :: Integer
    mintedValue = case flattenValue (txInfoMint info) of
      [(_, _, amt)] -> amt
      _ -> Ptx.traceError "must mint one token only"

    tn :: TokenName
    tn = sTokenName policySig

    pkh :: PubKeyHash
    pkh = sUser policySig

    vh :: ValidatorHash
    vh = sScript policySig

    findAccountDatumWithDH :: DatumHash -> Maybe AccountDatum
    findAccountDatumWithDH dh = do
      Datum d <- (`findDatum` info) dh
      PlutusTx.fromBuiltinData d

    scriptOutputs :: [(ValidatorHash, AccountDatum, Value)]
    scriptOutputs = foldr flt [] (txInfoOutputs info)
      where
        flt :: TxOut -> [(ValidatorHash, AccountDatum, Value)] -> [(ValidatorHash, AccountDatum, Value)]
        flt
          TxOut
            { txOutDatumHash = Just dh,
              txOutAddress = Address (ScriptCredential vh') _,
              txOutValue = v
            }
          acc = case findAccountDatumWithDH dh of
            Just ad -> (vh', ad, v) : acc
            _ -> acc
        flt _ acc = acc

    checkOutputs :: Bool
    checkOutputs = case scriptOutputs of
      [(vh', dat, v)] ->
        traceIfFalse "wrong datum" (dat == initDatum)
          && traceIfFalse "wrong amount" (v `geq` (singleton (ownCurrencySymbol ctx) tn mintedValue <> fees))
          && traceIfFalse "wrong script address" (vh == vh')
      _ -> traceError "script output should be exactly one"

policy :: Value -> Scripts.MintingPolicy
policy fees =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode fees

curSymbol :: Value -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy
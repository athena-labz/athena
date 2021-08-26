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

module Membership.OffChain.Signature where

import qualified Control.Monad as M
import Data.Monoid (Last (Last))
import Data.Text (Text)
import qualified Ledger as L
import Ledger.Constraints as Constraints
  ( mintingPolicy,
    mustMintValue,
    mustPayToOtherScript,
  )
import Ledger.Contexts (pubKeyHash)
import Ledger.Scripts (Datum (Datum))
import Ledger.Value as Value (assetClassValue, singleton)
import Membership.Account (AccountType, initDatum)
import Membership.OnChain.Account
import Membership.OnChain.Signature
import Membership.PlatformSettings
  ( IncompletePlatformSettings (ipsEntranceFee, ipsToken),
    PlatformSettings (..),
    completePS,
  )
import Membership.Signature (makeSigToken)
import Plutus.Contract as Contract
  ( Contract,
    Endpoint,
    Promise (awaitPromise),
    awaitTxConfirmed,
    endpoint,
    handleError,
    logError,
    logInfo,
    ownPubKey,
    submitTxConstraintsWith,
    tell
  )
import qualified PlutusTx
import PlutusTx.Prelude (($), (++), (<$>), (<>), Maybe(Just))
import Text.Printf (printf)
import qualified Prelude as P

type SigSchema = Endpoint "mint" IncompletePlatformSettings

mint :: IncompletePlatformSettings -> Contract (Last PlatformSettings) SigSchema Text ()
mint ips = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  let fees = assetClassValue (ipsToken ips) (ipsEntranceFee ips)
      cs = curSymbol fees
      ps = completePS cs ips

      vh = accountValidatorHash ps
      tn = makeSigToken pkh vh

  Contract.logInfo @P.String $ "public key hash: " ++ P.show pkh
  Contract.logInfo @P.String $ "validator hash: " ++ P.show vh
  Contract.logInfo @P.String $ "token name: " ++ P.show tn

  let mintVal = singleton cs tn 100
      lookups = Constraints.mintingPolicy (policy fees)
      tx =
        Constraints.mustMintValue mintVal
          P.<> Constraints.mustPayToOtherScript
            vh
            (Datum $ PlutusTx.toBuiltinData initDatum)
            (mintVal <> fees)

  ledgerTx <- submitTxConstraintsWith @AccountType lookups tx
  M.void $ awaitTxConfirmed $ L.txId ledgerTx
  tell $ Last $ Just ps
  Contract.logInfo @P.String $ printf "forged %s" (P.show mintVal)

endpoints :: Contract (Last PlatformSettings) SigSchema Text ()
endpoints =
  M.forever $
    handleError logError $
      awaitPromise $
        endpoint @"mint" $ \ips -> mint ips
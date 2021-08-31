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
import Ledger.Scripts
import Ledger.Value as Value (assetClassValue, singleton)
import Membership.Account (AccountType, initDatum)
import Membership.OnChain.Account
import Membership.OnChain.Contract
import Membership.OnChain.Signature
import Membership.PlatformSettings
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

type SigSchema = Endpoint "mint" PlatformSettings

mint :: PlatformSettings -> Contract (Last AccountSettings) SigSchema Text ()
mint ps = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  let fees = assetClassValue (psToken ps) (psEntranceFee ps)
      sigSym = curSymbol fees
      contrSett = ContractSettings
        { csPlatformToken = psToken ps,
          csSignatureSymbol = sigSym
        }
      contrValHash = validatorHash $ contractValidator contrSett
      as = AccountSettings
        { asPlatformSettings = ps,
          asSignatureSymbol = sigSym,
          asContractValidatorHash = contrValHash,
          asCollectors = [] -- Chnage that later
        }
      vh = accountValidatorHash as
      tn = makeSigToken pkh vh

  Contract.logInfo @P.String $ "public key hash: " ++ P.show pkh
  Contract.logInfo @P.String $ "validator hash: " ++ P.show vh
  Contract.logInfo @P.String $ "token name: " ++ P.show tn

  let mintVal = singleton sigSym tn 100
      lookups = Constraints.mintingPolicy (policy fees)
      tx =
        Constraints.mustMintValue mintVal
          P.<> Constraints.mustPayToOtherScript
            vh
            (Datum $ PlutusTx.toBuiltinData initDatum)
            (mintVal <> fees)

  ledgerTx <- submitTxConstraintsWith @AccountType lookups tx
  M.void $ awaitTxConfirmed $ L.txId ledgerTx
  tell $ Last $ Just as
  Contract.logInfo @P.String $ printf "forged %s" (P.show mintVal)

endpoints :: Contract (Last AccountSettings) SigSchema Text ()
endpoints =
  M.forever $
    handleError logError $
      awaitPromise $
        endpoint @"mint" $ \ps -> mint ps
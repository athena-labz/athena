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

module Membership.OffChain.Account where

import qualified Control.Monad as Monad
import qualified Data.Map as Map
import Data.Monoid (Last (Last))
import Data.Monoid as M (Last (Last), Monoid (mconcat))
import Data.Text (Text)
import qualified Ledger as L
import Ledger.Constraints as Constraints
import Ledger.Contexts (pubKeyHash)
import Ledger.Scripts
import Ledger.Typed.Scripts (ValidatorTypes (..))
import Ledger.Value as Value
  ( AssetClass,
    CurrencySymbol,
    TokenName,
    Value,
    assetClassValue,
    singleton,
  )
import Membership.Account
import Membership.OffChain.Utils
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
    select,
    submitTxConstraintsWith,
    tell,
    type (.\/),
  )
import qualified PlutusTx
import PlutusTx.Prelude
  ( Maybe (Just),
    fst,
    length,
    return,
    ($),
    (++),
    (<$>),
    (<>),
  )
import Text.Printf (printf)
import qualified Prelude as P

type AccountSchema =
  Endpoint "create-account" PlatformSettings
    .\/ Endpoint "collect-fees" AccountSettings

-- getAccountSettings get's the essential information from platform settings
-- needed for the account validator
getAccountSettings :: PlatformSettings -> AccountSettings
getAccountSettings platformSettings =
  AccountSettings
    { asPlatformSettings = platformSettings,
      asSignatureSymbol = sigSymbol,
      asContractValidatorHash = contrValHash
    }
  where
    -- The token used for every transaction in the platform (DSET)
    token :: AssetClass
    token = psToken platformSettings

    -- The currency symbol of all SIG tokens we build
    sigSymbol :: CurrencySymbol
    sigSymbol = signatureCurrencySymbol platformSettings

    -- The contract essential information
    contrSett :: ContractSettings
    contrSett =
      ContractSettings
        { csPlatformSettings = platformSettings,
          csSignatureSymbol = sigSymbol
        }

    -- The validator hash of the contract based on the settings we made
    contrValHash :: ValidatorHash
    contrValHash = validatorHash (contractValidator contrSett)

-- Create account, mint's 100 SIG tokens with the user's public key hash
-- embeded on and transfers it directly to a new account UTxO, therefore
-- "creating a new account"
createAccount :: PlatformSettings -> Contract () AccountSchema Text ()
createAccount ps = do
  -- The public key hash from the user who is trying to create an account
  pkh <- pubKeyHash <$> Contract.ownPubKey

  let -- The currency symbol we'll use to create the SIG token
      sigSymbol :: CurrencySymbol
      sigSymbol = signatureCurrencySymbol ps

      -- The essential information we need to know in order to create an account
      accountSettings :: AccountSettings
      accountSettings = getAccountSettings ps

      -- The validator hash from the all the platform accounts derived from the
      -- given settings
      accValHash :: ValidatorHash
      accValHash = accountValidatorHash accountSettings

      -- The token name, which is going to be used to build our SIG token
      sigTokenName :: TokenName
      sigTokenName = makeSigToken pkh accValHash

      entranceFeeValue, sigTokensValue :: Value
      -- The value needed to pay for the platform in order to create an account
      entranceFeeValue = assetClassValue (psToken ps) (psEntranceFee ps)
      -- The sig tokens which will me minted with the user's public key hash
      -- and the account validator hash embeded
      sigTokensValue = singleton sigSymbol sigTokenName 100

      lookups :: ScriptLookups AccountType
      lookups = Constraints.mintingPolicy (signaturePolicy ps)

      tx :: TxConstraints (RedeemerType AccountType) (DatumType AccountType)
      tx =
        Constraints.mustMintValue sigTokensValue
          P.<> Constraints.mustPayToOtherScript
            accValHash
            (Datum $ PlutusTx.toBuiltinData initDatum)
            (sigTokensValue <> entranceFeeValue)

  -- Submits the transaction to the blockchain
  ledgerTx <- submitTxConstraintsWith @AccountType lookups tx

  -- Waits for the transaction to be confirmed
  Monad.void $ awaitTxConfirmed $ L.txId ledgerTx

  Contract.logInfo
    @P.String
    $ printf "%s successfully created account" (P.show pkh)

-- collect fees should transfer all fees stored inside the account scripts
-- to the governance script
collectFees :: AccountSettings -> Contract w s Text ()
collectFees accountSettings =
  Contract.logError @P.String "Collect Fees - Incomplete"

accountEndpoints :: Contract () AccountSchema Text ()
accountEndpoints =
  Monad.forever $
    handleError logError $
      awaitPromise $
        createAccount' `select` collectFees'
  where
    createAccount' = endpoint @"create-account" $ \ps -> createAccount ps
    collectFees' = endpoint @"collect-fees" $ \as -> collectFees as
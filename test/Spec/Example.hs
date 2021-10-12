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

module Spec.Example where

import Control.Monad (void)
import Control.Monad.Freer.Extras as Extras (logError, logInfo)
import Data.Default (Default (..))
import qualified Data.Map as Map
import Data.Monoid (Last (..))
import Data.Text (Text, pack)
import Ledger
import Ledger.Ada as Ada (lovelaceValueOf)
import Ledger.TimeSlot
import Ledger.Value (AssetClass (AssetClass), assetClass, assetClassValue)
import Membership.Contract
import Membership.Logic
import Membership.OffChain.Account
import Membership.OffChain.Contract
import Membership.OffChain.Logic
import Membership.OnChain.Account
import Membership.OnChain.Contract
import Membership.OnChain.Logic
import Membership.OnChain.ShameToken
import Membership.OnChain.Signature
import Membership.PlatformSettings
import Membership.Service
import Membership.ShameToken
import Plutus.Contract.Test (Wallet (Wallet), knownWallet, walletPubKey)
import Plutus.Trace.Emulator as Emulator
  ( ContractHandle,
    EmulatorConfig (EmulatorConfig),
    EmulatorTrace,
    activateContractWallet,
    callEndpoint,
    observableState,
    runEmulatorTraceIO',
    waitNSlots,
  )
import PlutusTx.AssocMap as M
import PlutusTx.Prelude
  ( Bool (..),
    BuiltinByteString,
    Either (Left),
    Integer,
    Maybe (Just, Nothing),
    Semigroup ((<>)),
    ($),
    (++),
    (-),
  )
import qualified PlutusTx.Ratio as R
import Spec.Sample
import Spec.Trace
import Test.Tasty ()
import Prelude (IO, Show (..), String)
import qualified Prelude

createAccountExample :: EmulatorTrace ()
createAccountExample = do
  let alice = knownWallet 1

  -- Alice activates her accountEndpoints handler (this is completely off-chain)
  aliceAccountHandler <- activateContractWallet alice accountEndpoints

  -- Alice creates an account by paying an entrance fee
  -- This means that 100 SIG tokens are minted and transferred to the account script
  -- These tokens contain Alice's public key hash and now she can execute a series of
  -- actions with this account by requesting the script to transfer these tokens
  createAccountTrace aliceAccountHandler platformSettings

  void $ Emulator.waitNSlots 1

  -- Display Alice's account information
  accountInfoTrace aliceAccountHandler accountSettings

createContractExample :: EmulatorTrace ()
createContractExample = do
  let alice = knownWallet 1 -- Alice will be our fake service provider

  -- Alice activates her accountEndpoints handler (this is completely off-chain)
  aliceAccountHandler <- activateContractWallet alice accountEndpoints

  createAccountTrace aliceAccountHandler platformSettings

  void $ Emulator.waitNSlots 1

  -- Alice now creates a new contract
  -- Anyone can therefore sign this contract, promising to follow the rules defined
  -- by her, by transfering a collateral (called) trust and the price (if there is one)
  createContractTrace accountSettings sampleContractDatum alice
  void $ Emulator.waitNSlots 1

  -- Display Alice's account information
  accountInfoTrace aliceAccountHandler accountSettings

createLogicExample :: EmulatorTrace ()
createLogicExample = do
  let alice = knownWallet 1

  aliceAccountHandler <- activateContractWallet alice accountEndpoints

  createAccountTrace aliceAccountHandler platformSettings

  void $ Emulator.waitNSlots 1

  -- Alice creates a new contract, her role is, therefore, "Publisher"
  maybeContractNFT <- createContractTrace accountSettings sampleContractDatum alice

  case maybeContractNFT of
    Just contractNFT -> do
      Extras.logInfo $ "Contract created " ++ show contractNFT

      -- Alice creates a logic script
      -- This is what will be used to make accusations, it will also distribute the collateral
      -- according to the logic defined and the verdict from the judge
      void $ createLogicTrace accountSettings logicSettings key alice

      -- Display Alice's account information
      accountInfoTrace aliceAccountHandler accountSettings
    Nothing -> Extras.logError @String "Error creating account"

signContractExample :: EmulatorTrace ()
signContractExample = do
  let alice = knownWallet 1
      bob = knownWallet 2 -- Bob will be our fake client

  -- Alice and Bob activate their accountEndpoints handler
  -- (this is completely off-chain)
  aliceAccountHandler <- activateContractWallet alice accountEndpoints

  bobAccountHandler <- activateContractWallet bob accountEndpoints

  createAccountsTrace
    [aliceAccountHandler, bobAccountHandler]
    platformSettings

  void $ Emulator.waitNSlots 1

  -- Alice creates a new contract
  maybeContractNFT <- createContractTrace accountSettings sampleContractDatum alice

  case maybeContractNFT of
    Just contractNFT -> do
      Extras.logInfo $ "Contract created " ++ show contractNFT

      -- Alice creates a logic script
      void $ createLogicTrace accountSettings logicSettings key alice

      -- Bob should only sign the contract if the logic script was
      -- already created, so he can know the inputs are fair

      -- Bob activates his contractEndpoints handler (this is completely off-chain)
      bobContractHandler <- activateContractWallet bob contractEndpoints

      -- Bob signs Alice's contract as a Client
      signTrace bobContractHandler accountSettings contractNFT Client

      -- Display Alice's account information
      accountInfoTrace aliceAccountHandler accountSettings

      -- Display Bob's account information
      accountInfoTrace bobAccountHandler accountSettings
    Nothing -> Extras.logError @String "Error creating account"

accuseExample :: EmulatorTrace ()
accuseExample = do
  let alice = knownWallet 1
      bob = knownWallet 2
      charlie = knownWallet 3 -- Charlie will be our judge (he's public key can be found in the contract)
  Extras.logInfo $ "Alice Public Key " ++ show (pubKeyHash $ walletPubKey alice)
  Extras.logInfo $ "Bob Public Key " ++ show (pubKeyHash $ walletPubKey bob)
  Extras.logInfo $ "Charlie Public Key " ++ show (pubKeyHash $ walletPubKey charlie)

  -- Alice, Bob and Charlie activate their accountEndpoints handler
  -- (this is completely off-chain)
  aliceAccountHandler <- activateContractWallet alice accountEndpoints

  bobAccountHandler <- activateContractWallet bob accountEndpoints

  charlieAccountHandler <- activateContractWallet charlie accountEndpoints

  createAccountsTrace
    [aliceAccountHandler, bobAccountHandler, charlieAccountHandler]
    platformSettings

  void $ Emulator.waitNSlots 1

  -- Alice creates a new contract
  maybeContractNFT <- createContractTrace accountSettings sampleContractDatum alice

  case maybeContractNFT of
    Just contractNFT -> do
      Extras.logInfo $ "Contract created " ++ show contractNFT

      -- Alice creates a logic script and stores the shame token
      -- asset class returned to be later used
      maybeShameToken <- createLogicTrace accountSettings logicSettings key alice

      -- Bob should only sign the contract if the logic script was
      -- already created, so he can know the inputs are fair

      -- Bob activates his contractEndpoints handler (this is completely off-chain)
      bobContractHandler <- activateContractWallet bob contractEndpoints

      -- Charlie also activates his contract handler, so he can sign the contract
      charlieContractHandler <- activateContractWallet charlie contractEndpoints

      -- Bob signs Alice's contract as a Client
      signTrace bobContractHandler accountSettings contractNFT Client

      -- Charlie also signs Alice's contract, but as a judge
      signTrace charlieContractHandler accountSettings contractNFT Mediator

      -- Now that we have at least one judge, people can start accusing each other
      case maybeShameToken of
        -- If the Alice transaction succeded, we can get the shame token
        -- returned to execute multiple actions with the logic script
        Just shameToken -> do
          -- Now that we are going to transact with the logic script,
          -- Bob needs a logic handler
          bobLogicHandler <- activateContractWallet bob logicEndpoints

          -- Bob accuses alice, the reason is not specified, this can be done off-chain
          -- by communicating with the judge (Charlie)
          -- For that he had to pay the judge price specified in the contract
          accuseTrace
            bobLogicHandler
            (pubKeyHash $ walletPubKey alice) -- Alice's public key hash
            accountSettings
            logicSettings
            contractNFT
            shameToken

          -- Display Alice's account information
          accountInfoTrace aliceAccountHandler accountSettings

          -- Display Bob's account information
          accountInfoTrace bobAccountHandler accountSettings
    Nothing -> Extras.logError @String "Error creating account"

mediateExample :: EmulatorTrace ()
mediateExample = do
  let alice = knownWallet 1
      bob = knownWallet 2
      charlie = knownWallet 3

  Extras.logInfo $ "Alice Public Key " ++ show (pubKeyHash $ walletPubKey alice)
  Extras.logInfo $ "Bob Public Key " ++ show (pubKeyHash $ walletPubKey bob)
  Extras.logInfo $ "Charlie Public Key " ++ show (pubKeyHash $ walletPubKey charlie)

  -- Alice, Bob and Charlie activate their accountEndpoints handler
  -- (this is completely off-chain)
  aliceAccountHandler <- activateContractWallet alice accountEndpoints

  bobAccountHandler <- activateContractWallet bob accountEndpoints

  charlieAccountHandler <- activateContractWallet charlie accountEndpoints

  createAccountsTrace
    [aliceAccountHandler, bobAccountHandler, charlieAccountHandler]
    platformSettings

  void $ Emulator.waitNSlots 1

  -- Alice creates a new contract
  maybeContractNFT <- createContractTrace accountSettings sampleContractDatum alice

  case maybeContractNFT of
    Just contractNFT -> do
      Extras.logInfo $ "Contract created " ++ show contractNFT

      -- Alice creates a logic script and stores the shame token
      -- asset class returned to be later used
      maybeShameToken <- createLogicTrace accountSettings logicSettings key alice

      -- Bob should only sign the contract if the logic script was
      -- already created, so he can know the inputs are fair

      -- Bob activates his contractEndpoints handler (this is completely off-chain)
      bobContractHandler <- activateContractWallet bob contractEndpoints

      -- Charlie also activates his contract handler, so he can sign the contract
      charlieContractHandler <- activateContractWallet charlie contractEndpoints

      -- Bob signs Alice's contract as a Client
      signTrace bobContractHandler accountSettings contractNFT Client

      -- Charlie also signs Alice's contract, but as a judge
      signTrace charlieContractHandler accountSettings contractNFT Mediator

      -- Now that we have at least one judge, people can start accusing each other
      case maybeShameToken of
        -- If the Alice transaction succeded, we can get the shame token
        -- returned to execute multiple actions with the logic script
        Just shameToken -> do
          -- Now that we are going to transact with the logic script,
          -- Bob needs a logic handler
          bobLogicHandler <- activateContractWallet bob logicEndpoints

          -- Bob accuses alice, the reason is not specified, this can be done off-chain
          -- by communicating with the judge (Charlie)
          -- For that he had to pay the judge price specified in the contract
          accuseTrace
            bobLogicHandler
            (pubKeyHash $ walletPubKey alice) -- Alice's public key hash
            accountSettings
            logicSettings
            contractNFT
            shameToken

          -- For Charlie to mediate this conflict, he also needs a logic handler
          charlieLogicHandler <- activateContractWallet charlie logicEndpoints

          -- After Bob talked with both parties, getting all the information he
          -- could in order to mediate this conflict, he will send a verdict, which
          -- is nothing more than a series of answers to a bunch of questions, which
          -- will be analyzed by the logic to distribute the trust tokens accordingly
          let verdict :: Verdict
              verdict =
                M.fromList
                  [ (input1, True),
                    (input2, True),
                    (input3, False),
                    (input4, True),
                    (input5, False)
                  ]

          -- Charlie mediates the conflict from the logic identified
          -- by this specific shame token
          mediateTrace
            charlieLogicHandler
            verdict
            accountSettings
            logicSettings
            shameToken
        

          -- Display Alice's account information
          accountInfoTrace aliceAccountHandler accountSettings

          -- Display Bob's account information
          accountInfoTrace bobAccountHandler accountSettings
          
          -- Display Charlie's account information
          accountInfoTrace charlieAccountHandler accountSettings
    Nothing -> Extras.logError @String "Error creating account"

logicCollectExample :: EmulatorTrace ()
logicCollectExample = do
  let alice = knownWallet 1
      bob = knownWallet 2
      charlie = knownWallet 3

  Extras.logInfo $ "Alice Public Key " ++ show (pubKeyHash $ walletPubKey alice)
  Extras.logInfo $ "Bob Public Key " ++ show (pubKeyHash $ walletPubKey bob)
  Extras.logInfo $ "Charlie Public Key " ++ show (pubKeyHash $ walletPubKey charlie)

  -- Alice, Bob and Charlie activate their accountEndpoints handler
  -- (this is completely off-chain)
  aliceAccountHandler <- activateContractWallet alice accountEndpoints

  bobAccountHandler <- activateContractWallet bob accountEndpoints

  charlieAccountHandler <- activateContractWallet charlie accountEndpoints

  createAccountsTrace
    [aliceAccountHandler, bobAccountHandler, charlieAccountHandler]
    platformSettings

  void $ Emulator.waitNSlots 1

  -- Alice creates a new contract
  maybeContractNFT <- createContractTrace accountSettings sampleContractDatum alice

  case maybeContractNFT of
    Just contractNFT -> do
      Extras.logInfo $ "Contract created " ++ show contractNFT

      -- Alice creates a logic script and stores the shame token
      -- asset class returned to be later used
      maybeShameToken <- createLogicTrace accountSettings logicSettings key alice

      -- Bob should only sign the contract if the logic script was
      -- already created, so he can know the inputs are fair

      -- Bob activates his contractEndpoints handler (this is completely off-chain)
      bobContractHandler <- activateContractWallet bob contractEndpoints

      -- Charlie also activates his contract handler, so he can sign the contract
      charlieContractHandler <- activateContractWallet charlie contractEndpoints

      -- Bob signs Alice's contract as a Client
      signTrace bobContractHandler accountSettings contractNFT Client

      -- Charlie also signs Alice's contract, but as a judge
      signTrace charlieContractHandler accountSettings contractNFT Mediator

      -- Now that we have at least one judge, people can start accusing each other
      case maybeShameToken of
        -- If the Alice transaction succeded, we can get the shame token
        -- returned to execute multiple actions with the logic script
        Just shameToken -> do
          -- Now that we are going to transact with the logic script,
          -- Bob needs a logic handler
          bobLogicHandler <- activateContractWallet bob logicEndpoints

          -- Bob accuses alice, the reason is not specified, this can be done off-chain
          -- by communicating with the judge (Charlie)
          -- For that he had to pay the judge price specified in the contract
          accuseTrace
            bobLogicHandler
            (pubKeyHash $ walletPubKey alice) -- Alice's public key hash
            accountSettings
            logicSettings
            contractNFT
            shameToken

          -- For Charlie to mediate this conflict, he also needs a logic handler
          charlieLogicHandler <- activateContractWallet charlie logicEndpoints

          -- After Bob talked with both parties, getting all the information he
          -- could in order to mediate this conflict, he will send a verdict, which
          -- is nothing more than a series of answers to a bunch of questions, which
          -- will be analyzed by the logic to distribute the trust tokens accordingly
          let verdict :: Verdict
              verdict =
                M.fromList
                  [ (input1, True),
                    (input2, True),
                    (input3, False),
                    (input4, True),
                    (input5, False)
                  ]

          -- Charlie mediates the conflict from the logic identified
          -- by this specific shame token. Alice is declared guilty.
          mediateTrace
            charlieLogicHandler
            verdict
            accountSettings
            logicSettings
            shameToken

          -- Bob activates the logic script distribution after the conflict has
          -- been mediated, since he will earn Alice's trust tokens
          -- Alice is out of the contract becasue was declared guilty by the logic script
          logicCollectTrace
            bobLogicHandler
            accountSettings
            logicSettings
            contractNFT
            shameToken
        

          -- Display Alice's account information
          accountInfoTrace aliceAccountHandler accountSettings

          -- Display Bob's account information
          accountInfoTrace bobAccountHandler accountSettings
          
          -- Display Charlie's account information
          accountInfoTrace charlieAccountHandler accountSettings
    Nothing -> Extras.logError @String "Error creating account"

leaveContractExample :: EmulatorTrace ()
leaveContractExample = do
  let alice = knownWallet 1
      bob = knownWallet 2
      charlie = knownWallet 3

  Extras.logInfo $ "Alice Public Key " ++ show (pubKeyHash $ walletPubKey alice)
  Extras.logInfo $ "Bob Public Key " ++ show (pubKeyHash $ walletPubKey bob)
  Extras.logInfo $ "Charlie Public Key " ++ show (pubKeyHash $ walletPubKey charlie)

  -- Alice, Bob and Charlie activate their accountEndpoints handler
  -- (this is completely off-chain)
  aliceAccountHandler <- activateContractWallet alice accountEndpoints

  bobAccountHandler <- activateContractWallet bob accountEndpoints

  charlieAccountHandler <- activateContractWallet charlie accountEndpoints

  createAccountsTrace
    [aliceAccountHandler, bobAccountHandler, charlieAccountHandler]
    platformSettings

  void $ Emulator.waitNSlots 1

  -- Alice creates a new contract
  maybeContractNFT <- createContractTrace accountSettings sampleContractDatum alice

  case maybeContractNFT of
    Just contractNFT -> do
      Extras.logInfo $ "Contract created " ++ show contractNFT

      -- Alice creates a logic script and stores the shame token
      -- asset class returned to be later used
      maybeShameToken <- createLogicTrace accountSettings logicSettings key alice

      -- Bob should only sign the contract if the logic script was
      -- already created, so he can know the inputs are fair

      -- Bob activates his contractEndpoints handler
      bobContractHandler <- activateContractWallet bob contractEndpoints

      -- Charlie also activates his contract handler, so he can sign the contract
      charlieContractHandler <- activateContractWallet charlie contractEndpoints

      -- Bob signs Alice's contract as a Client
      signTrace bobContractHandler accountSettings contractNFT Client

      -- Charlie also signs Alice's contract, but as a judge
      signTrace charlieContractHandler accountSettings contractNFT Mediator

      -- Now that we have at least one judge, people can start accusing each other
      case maybeShameToken of
        -- If the Alice transaction succeded, we can get the shame token
        -- returned to execute multiple actions with the logic script
        Just shameToken -> do
          -- Now that we are going to transact with the logic script,
          -- Bob needs a logic handler
          bobLogicHandler <- activateContractWallet bob logicEndpoints

          -- Bob accuses alice, the reason is not specified, this can be done off-chain
          -- by communicating with the judge (Charlie)
          -- For that he had to pay the judge price specified in the contract
          accuseTrace
            bobLogicHandler
            (pubKeyHash $ walletPubKey alice) -- Alice's public key hash
            accountSettings
            logicSettings
            contractNFT
            shameToken

          -- For Charlie to mediate this conflict, he also needs a logic handler
          charlieLogicHandler <- activateContractWallet charlie logicEndpoints

          -- After Bob talked with both parties, getting all the information he
          -- could in order to mediate this conflict, he will send a verdict, which
          -- is nothing more than a series of answers to a bunch of questions, which
          -- will be analyzed by the logic to distribute the trust tokens accordingly
          let verdict :: Verdict
              verdict =
                M.fromList
                  [ (input1, True),
                    (input2, True),
                    (input3, False),
                    (input4, True),
                    (input5, False)
                  ]

          -- Charlie mediates the conflict from the logic identified
          -- by this specific shame token. Alice is declared guilty.
          mediateTrace
            charlieLogicHandler
            verdict
            accountSettings
            logicSettings
            shameToken

          -- Bob activates the logic script distribution after the conflict has
          -- been mediated, since he will earn Alice's trust tokens
          -- Alice is out of the contract becasue was declared guilty by the logic script
          logicCollectTrace
            bobLogicHandler
            accountSettings
            logicSettings
            contractNFT
            shameToken

          -- After Bob had a really bad experience with Alice, he might want to
          -- leave the contract and get his collateral back
          clientLeaveTrace
            bobContractHandler
            accountSettings
            contractNFT
            (sampleReview (pubKeyHash $ walletPubKey bob))
            (pubKeyHash $ walletPubKey alice)


          -- Charlie might do the same
          leaveTrace
            charlieContractHandler
            accountSettings
            contractNFT

          -- Display Alice's account information
          accountInfoTrace aliceAccountHandler accountSettings

          -- Display Bob's account information
          accountInfoTrace bobAccountHandler accountSettings
          
          -- Display Charlie's account information
          accountInfoTrace charlieAccountHandler accountSettings
    Nothing -> Extras.logError @String "Error creating account"
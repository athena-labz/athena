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

module Membership.OnChain.Account where

import Ledger
  ( Address,
    PubKeyHash,
    ScriptContext (scriptContextTxInfo),
    TxInfo,
    TxOut (txOutValue),
    Validator,
    ValidatorHash,
    findDatum,
    ownHash,
    scriptAddress,
    txSignedBy,
    validatorHash,
  )
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value
  ( AssetClass (AssetClass),
    CurrencySymbol,
    TokenName (TokenName),
    Value,
    assetClassValue,
    assetClassValueOf,
    flattenValue,
    geq,
  )
import Membership.Account
  ( AccountDatum (..),
    AccountRedeemer (..),
    AccountReturnType (..),
    AccountType,
    addContract,
    cancelContractCAS,
    contractCreationCAS,
    declaredGuiltyCAS,
    findAccountDatum,
    leaveContractCAS,
    removeContract,
    signContractCAS,
  )
import Membership.Contract
  ( Accusation (..),
    ContractDatum (..),
    findContractDatum,
    findContractNFT,
    isInitial,
  )
import Membership.Logic
  ( LogicState (..),
    findLogicDatum,
    findShameTokenAssetClass,
  )
import Membership.OnChain.Utils
  ( sigTokenInContext,
    strictFindOutAndIn,
  )
import Membership.PlatformSettings
import Membership.Service
import Membership.Signature
import Membership.Utils
  ( strictFindInputWithValHash,
    strictFindOutputWithValHash,
  )
import qualified PlutusTx
import qualified PlutusTx.AssocMap as M
import PlutusTx.Prelude
  ( Bool (..),
    BuiltinString,
    Eq ((==)),
    Integer,
    Maybe (..),
    Semigroup ((<>)),
    find,
    fst,
    negate,
    otherwise,
    return,
    snd,
    traceError,
    traceIfFalse,
    ($),
    (&&),
    (.),
    (||),
  )

-- import Prelude (Semigroup (..))

-- The token name our contract NFTs will have
{-# INLINEABLE contractNFTTokenName #-}
contractNFTTokenName :: TokenName
contractNFTTokenName = TokenName "contract-nft"

-- Based on a validator hash and the information about a transaction, returns the
-- pair contract datum, value from the contract input we are consuming (if we
-- actually are)
{-# INLINEABLE findContractInput #-}
findContractInput :: ValidatorHash -> TxInfo -> Maybe (ContractDatum, Value)
findContractInput contrValHash info = do
  o <- strictFindInputWithValHash contrValHash info
  d <- findContractDatum o (`findDatum` info)
  return (d, txOutValue o)

-- Based on a validator hash and the information about a transaction, returns the
-- pair contract datum, value from the contract output we are consuming (if we
-- actually are)
{-# INLINEABLE findContractOutput #-}
findContractOutput :: ValidatorHash -> TxInfo -> Maybe (ContractDatum, Value)
findContractOutput contrValHash info = do
  o <- strictFindOutputWithValHash contrValHash info
  d <- findContractDatum o (`findDatum` info)
  return (d, txOutValue o)

-- Make sure the account UTxO in the input is present, that all the SIG tokens
-- have the same token name and that the transaction is signed by the user who
-- owns this SIG token
{-# INLINEABLE validateAccount #-}
validateAccount :: AccountSettings -> ScriptContext -> Bool
validateAccount (AccountSettings _ sigSymbol _) ctx =
  traceIfFalse
    "Account - SIG token missing in input"
    (sigTokenInContext sigToken ctx)
    && traceIfFalse
      "Account - Transaction not signed by account owner"
      (txSignedBy info pkh)
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx

    pkh :: PubKeyHash
    pkh = case findSignatory sigSymbol (txOutValue ownInput) of
      Just pkh' -> pkh'
      Nothing -> traceError "Account - No SIG token found in input"

    sigToken :: AssetClass
    sigToken = signatureAssetClass sigSymbol pkh (ownHash ctx)

{-# INLINEABLE createTraceIfFalse #-}
createTraceIfFalse :: BuiltinString -> Bool -> Bool
createTraceIfFalse msg = traceIfFalse ("Account Create Contract - " <> msg)

{-# INLINEABLE createTraceError #-}
createTraceError :: forall a. BuiltinString -> a
createTraceError msg = traceError ("Account Create Contract - " <> msg)

{-# INLINEABLE validateCreateContract #-}
validateCreateContract ::
  AccountSettings ->
  AccountDatum ->
  ScriptContext ->
  Bool
validateCreateContract accountSettings inputDatum ctx =
  createTraceIfFalse
    "Transaction not signed by account owner"
    (txSignedBy info pkh)
    && createTraceIfFalse "Invalid account value" validAccountValue
    && createTraceIfFalse "Invalid account datum" validAccountDatum
    && createTraceIfFalse "Invalid contract value" validContractValue
    && createTraceIfFalse "Invalid contract datum" validContractDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    platformSettings :: PlatformSettings
    platformSettings = asPlatformSettings accountSettings

    -- The account UTxO in the input and output
    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    -- The Account Datum based on the output datum hash
    outputDatum :: AccountDatum
    outputDatum = case findAccountDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> createTraceError "Output Datum not found"

    sigSymbol :: CurrencySymbol
    sigSymbol = asSignatureSymbol accountSettings

    -- Tries to find the SIG tokens contained inside our UTxO input while making
    -- sure the validator hash embeded in the SIG token name is the same as our
    -- hash
    ownSig :: Sig
    ownSig = case findSignatures sigSymbol (txOutValue ownInput) of
      [s]
        | sScript s == ownHash ctx -> s
        | otherwise -> createTraceError "Invalid SIG hash"
      _ -> createTraceError "Should only have one SIG"

    -- Based on the SIG tokens we found, the account owner public key hash
    pkh :: PubKeyHash
    pkh = sUser ownSig

    -- Based on the SIG tokens we found construct a value that has an amount of 1
    sigValue :: Value
    sigValue = signatureValue' sigSymbol ownSig

    transactionFeeValue :: Value
    transactionFeeValue =
      assetClassValue
        (psToken platformSettings)
        (psTxFee platformSettings)

    -- Gets the platform contracts validator hash from the account settings
    contrValHash :: ValidatorHash
    contrValHash = asContractValidatorHash accountSettings

    -- Based on the validator hash we got from our settings, tries to find a
    -- contract UTxO within this transaction's output
    contractOutput :: TxOut
    contractOutput = case strictFindOutputWithValHash contrValHash info of
      Just o -> o
      Nothing ->
        createTraceError "Couldn't find an unique contract output"

    -- If we could find the UTxO from the contract, get it's datum
    contractDatum :: ContractDatum
    contractDatum = case findContractDatum contractOutput (`findDatum` info) of
      Just dat -> dat
      Nothing ->
        createTraceError "- Contract Datum could not be found"

    -- Tries to find the contract NFT identifier
    contractNFT :: AssetClass
    contractNFT = case findContractNFT (txOutValue contractOutput) of
      Just ac -> ac
      Nothing -> createTraceError "No NFT found inside contract"

    -- Make the contract NFT into a value with amount 1
    contractNFTValue :: Value
    contractNFTValue = assetClassValue contractNFT 1

    -- The collateral our user is supposed to pay for the contract
    trustValue :: Value
    trustValue =
      assetClassValue
        (psToken platformSettings)
        (sTrust $ cdService contractDatum)

    -- What we expect our account output UTxO datum
    -- In this case it should add a contract to it's list and increase the CAS
    -- according to the CAS Map
    expectedOutputDatum :: AccountDatum
    expectedOutputDatum =
      addContract
        (contractCreationCAS (psCASMap platformSettings) inputDatum)
        contrValHash
        contractNFT

    -- Make sure our user paid the transactions fee and our account lost only
    -- one SIG token
    validAccountValue :: Bool
    validAccountValue =
      txOutValue ownOutput <> negate (txOutValue ownInput)
        == transactionFeeValue
        <> negate (signatureValue sigSymbol pkh (ownHash ctx))

    -- Make sure the account datum matches our expected value
    validAccountDatum :: Bool
    validAccountDatum = outputDatum == expectedOutputDatum

    -- Make sure our newly created contract has exactly our SIG token, our trust
    -- value (collateral) and the NFT identifier
    validContractValue :: Bool
    validContractValue =
      txOutValue contractOutput
        == sigValue
        <> trustValue
        <> contractNFTValue

    -- Make sure our contract data is initial (no accusation, more than one
    -- judge, etc)
    validContractDatum :: Bool
    validContractDatum = isInitial pkh contractDatum

{-# INLINEABLE signTraceIfFalse #-}
signTraceIfFalse :: BuiltinString -> Bool -> Bool
signTraceIfFalse msg = traceIfFalse ("Account Sign - " <> msg)

{-# INLINEABLE signTraceError #-}
signTraceError :: forall a. BuiltinString -> a
signTraceError msg = traceError ("Account Sign - " <> msg)

{-# INLINEABLE validateSign #-}
validateSign :: AccountSettings -> AccountDatum -> ScriptContext -> Bool
validateSign accountSettings inputDatum ctx =
  signTraceIfFalse "Invalid Account" (validateAccount accountSettings ctx)
    && signTraceIfFalse "SIG token was not paid to the contract" sigPaid
    && signTraceIfFalse "Final value does not match" valueMatches
    && signTraceIfFalse "Wrong account datum" validAccountDatum
    && signTraceIfFalse "Invalid contract" validContract
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Tries to find our account input and output UTxOs if they are unique
    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    -- Tries to get the account output datum based on it's datum hash and the
    -- transaction info
    outputDatum :: AccountDatum
    outputDatum = case findAccountDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> signTraceError "Could not find output account datum"

    sigSymbol :: CurrencySymbol
    sigSymbol = asSignatureSymbol accountSettings

    contrValHash :: ValidatorHash
    contrValHash = asContractValidatorHash accountSettings

    platformToken :: AssetClass
    platformToken = (psToken . asPlatformSettings) accountSettings

    transactionFee :: Integer
    transactionFee = (psTxFee . asPlatformSettings) accountSettings

    transactionFeeValue :: Value
    transactionFeeValue = assetClassValue platformToken transactionFee

    -- The public key hash from the person who owns this account and is trying
    -- to sign a contract
    pkh :: PubKeyHash
    pkh = case findSignatory sigSymbol (txOutValue ownInput) of
      Just pkh' -> pkh'
      Nothing -> signTraceError "No SIG token found in input"

    sigToken :: AssetClass
    sigToken = signatureAssetClass sigSymbol pkh (ownHash ctx)

    -- Tries to find a contract input and output UTxO and get's it's datum and
    -- value
    contractInput, contractOutput :: Maybe (ContractDatum, Value)
    contractInput = findContractInput contrValHash info
    contractOutput = findContractOutput contrValHash info

    -- Makes sure the contract output UTxO received our account SIG token
    sigPaid :: Bool
    sigPaid = case contractOutput of
      Just (_, val) -> assetClassValueOf val sigToken == 1
      _ -> signTraceError "No contract output"

    -- Tries to find the NFT identifier inside the contract input UTxO
    maybeContractNFTInput :: Maybe AssetClass
    maybeContractNFTInput = do
      (_, contractValue) <- contractInput
      (cs, tn, _) <-
        find
          (\(_, tn, amt) -> tn == contractNFTTokenName && amt == 1)
          (flattenValue contractValue)
      return $ AssetClass (cs, tn)

    -- Tries to find the NFT identifier inside the contract output UTxO
    maybeContractNFTOutput :: Maybe AssetClass
    maybeContractNFTOutput = do
      (_, contractValue) <- contractOutput
      (cs, tn, _) <-
        find
          (\(_, tn, amt) -> tn == contractNFTTokenName && amt == 1)
          (flattenValue contractValue)
      return $ AssetClass (cs, tn)

    -- The contract is only valid if it has the contract NFT in both it's input
    -- and output. This is done to ensure the NFT is alway present in the
    -- contract
    validContract :: Bool
    validContract = case (maybeContractNFTInput, maybeContractNFTOutput) of
      (Just _, Just _) -> True
      _ -> False

    -- The account must only earn the corresponding transaction fees and lose
    -- exactly one SIG token
    valueMatches :: Bool
    valueMatches =
      txOutValue ownOutput
        == ( txOutValue ownInput
               <> transactionFeeValue
               <> negate (signatureValue sigSymbol pkh (ownHash ctx))
           )

    -- The account datum must increase it's CAS (because we are signing a
    -- contract, meaning we are participating in the platform) and the contracts
    -- list must receive a new element (the contract we are consuming)
    validAccountDatum :: Bool
    validAccountDatum = case maybeContractNFTInput of
      Just contractNFT ->
        signContractCAS
          (psCASMap $ asPlatformSettings accountSettings)
          (addContract inputDatum contrValHash contractNFT)
          == outputDatum
      Nothing -> signTraceError "Contract NFT not found"

{-# INLINEABLE validateCollect #-}
validateCollect :: AccountSettings -> PubKeyHash -> ScriptContext -> Bool
validateCollect _ _ _ = traceError "Validate Collect - Incomplete"

{-# INLINEABLE validateReview #-}
validateReview :: AccountSettings -> AccountDatum -> ScriptContext -> Bool
validateReview _ _ _ = traceError "Validate Review - Incomplete"

{-# INLINEABLE expelledTraceIfFalse #-}
expelledTraceIfFalse :: BuiltinString -> Bool -> Bool
expelledTraceIfFalse msg = traceIfFalse ("Account Expelled - " <> msg)

{-# INLINEABLE expelledTraceError #-}
expelledTraceError :: forall a. BuiltinString -> a
expelledTraceError msg = traceError ("Account Expelled - " <> msg)

{-# INLINEABLE leaveTraceIfFalse #-}
leaveTraceIfFalse :: BuiltinString -> Bool -> Bool
leaveTraceIfFalse msg = traceIfFalse ("Account Leave - " <> msg)

{-# INLINEABLE leaveTraceError #-}
leaveTraceError :: forall a. BuiltinString -> a
leaveTraceError msg = traceError ("Account Leave - " <> msg)

validateReturn ::
  AccountSettings ->
  AccountReturnType ->
  AccountDatum ->
  ScriptContext ->
  Bool
validateReturn accountSettings ARTExpelled inputAccountDatum ctx =
  expelledTraceIfFalse "Invalid Contract" validContract
    && expelledTraceIfFalse "Invalid Logic" validLogic
    && expelledTraceIfFalse "Invalid Logic State" validLogicState
    && expelledTraceIfFalse "Invalid Account" validAccount
    && expelledTraceIfFalse "Invalid Account Value" validAccountValue
    && expelledTraceIfFalse "Invalid Account Datum" validAccountDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- Tries to find our account input and output UTxOs if they are unique
    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    -- Tries to get the account output datum based on it's datum hash and the
    -- transaction info
    outputAccountDatum :: AccountDatum
    outputAccountDatum = case findAccountDatum ownOutput (`findDatum` info) of
      Just ad -> ad
      Nothing -> expelledTraceError "Could not find account output datum"

    sigSymbol :: CurrencySymbol
    sigSymbol = asSignatureSymbol accountSettings

    -- The sig token from the account which is being consumed (the account from
    -- the user who is being expelled)
    expelledSig :: Sig
    expelledSig = case findSignatures sigSymbol (txOutValue ownInput) of
      [s] -> s
      _ -> expelledTraceError "Should only have one SIG"

    -- The public key hash from the user who is being expelled
    pkh :: PubKeyHash
    pkh = sUser expelledSig

    sigValue :: Value
    sigValue = signatureValue' sigSymbol expelledSig

    -- The validator has from all platform contracts
    contrValHash :: ValidatorHash
    contrValHash = asContractValidatorHash accountSettings

    -- The contract input UTxO from which the user is being expelled
    contractInput :: TxOut
    contractInput = case strictFindInputWithValHash contrValHash info of
      Just out -> out
      Nothing -> expelledTraceError "No contract being consumed"

    -- The datum from the contract we are consuming
    contractDatum :: ContractDatum
    contractDatum = case findContractDatum contractInput (`findDatum` info) of
      Just dat -> dat
      Nothing -> expelledTraceError "Contract Datum could not be found"

    -- The NFT which identifies the contract we are consuming
    contractNFT :: AssetClass
    contractNFT = case M.lookup contrValHash (adContracts inputAccountDatum) of
      Just ac -> ac
      Nothing -> expelledTraceError "Contract not registered"

    -- The logic validator hash this contract had
    logValHash :: ValidatorHash
    logValHash = cdLogicScript contractDatum

    -- The UTxO from the logic input, which should prove the user is being
    -- expelled
    logicInput :: TxOut
    logicInput = case strictFindInputWithValHash logValHash info of
      Just out -> out
      Nothing -> expelledTraceError "No logic being consumed"

    -- The data / state from the logic we are consuming
    logicDatum :: LogicState
    logicDatum = case findLogicDatum logicInput (`findDatum` info) of
      Just state -> state
      Nothing -> expelledTraceError "Logic datum not found"

    -- The accusation which we can find inside the logic state
    accusation :: Accusation
    accusation = case logicDatum of
      (LSWaitingEnd _ _ acc _) -> acc
      _ -> expelledTraceError "Logic in wrong state"

    -- The asset class from the shame token used to idenitfy the logic script
    shameTokenAssetClass :: AssetClass
    shameTokenAssetClass = case findShameTokenAssetClass
      (psShameTokenSymbol $ asPlatformSettings accountSettings)
      (txOutValue logicInput) of
      Just ac -> ac
      Nothing -> expelledTraceError "Shame Token could not be found"

    shameTokenValue :: Value
    shameTokenValue = assetClassValue shameTokenAssetClass 1

    -- Makes sure the contract is valid (contains the NFT identifier) and that
    -- it has been signed by our expelled user
    validContract :: Bool
    validContract = case findContractNFT (txOutValue contractInput) of
      Just ac ->
        ac == contractNFT
          && txOutValue contractInput `geq` sigValue
      Nothing -> False

    -- Makes sure our logic contains the shame token
    validLogic :: Bool
    validLogic = txOutValue logicInput `geq` shameTokenValue

    -- This is redundant since accusation verifies that already, but is here for
    -- clarity
    validLogicState :: Bool
    validLogicState = case logicDatum of
      LSWaitingEnd {} -> True
      _ -> False

    -- Makes sure our expelled user was the one being accused, if the logic
    -- validates and allows this transaction, it means our user was indeed
    -- guilty
    validAccount :: Bool
    validAccount =
      pkh == fst (aAccused accusation)
        && sScript expelledSig == ownHash ctx

    -- The account value must fit one of two cases:
    --   Receives only the sig token because our user break some rule, which was
    -- not so serious
    --   Receives the sig token and the shame token
    validAccountValue :: Bool
    validAccountValue =
      (valueDifference == sigValue <> shameTokenValue)
        || (valueDifference == sigValue)
      where
        valueDifference :: Value
        valueDifference = txOutValue ownOutput <> negate (txOutValue ownInput)

    -- The expelled user new account datum must have it's CAS decreased (since
    -- he break the contract rules) and must remove the contract from it's list
    -- (since he was expelled)
    validAccountDatum :: Bool
    validAccountDatum =
      outputAccountDatum
        == declaredGuiltyCAS
          (psCASMap $ asPlatformSettings accountSettings)
          (removeContract inputAccountDatum contrValHash)

-- Leaving or Cancelling ->
--    Transactions should be signed by the account owner
--    Account should be valid (at least one sig token and all
-- from the same person and with the right validator hash);
--    The contract it's consuming should be in our list and should contain the NFT;
--    The value difference should be equal to the user SIG token;
--    The datum should be the same, except:
--       The CAS should increase or decrease according to the CASMap and the type;
--       The contract consumed should be removed from the list;
validateReturn accountSettings returnType inputDatum ctx =
  leaveTraceIfFalse "Not signed by account owner" (txSignedBy info pkh)
    && leaveTraceIfFalse "Invalid Account" validAccount
    && leaveTraceIfFalse "Invalid Account Value" validAccountValue
    && leaveTraceIfFalse "Invalid Account Datum" validAccountDatum
    && leaveTraceIfFalse "Invalid Contract" validContract
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    platformSettings :: PlatformSettings
    platformSettings = asPlatformSettings accountSettings

    -- casMap determines how much each action should affect the account cas score
    casMap :: CASMap
    casMap = psCASMap platformSettings

    -- Our account input and output UTxO
    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    -- The data contained inside our account output UTxO
    accountDatum :: AccountDatum
    accountDatum = case findAccountDatum ownOutput (`findDatum` info) of
      Just ad -> ad
      Nothing -> leaveTraceError "Could not find account output datum"

    sigSymbol :: CurrencySymbol
    sigSymbol = asSignatureSymbol accountSettings

    -- A digestable representation of our sig token, should have emebeded inside
    -- the validator hash from the account we are consuming and should be unique
    -- (only one user, but can have an amount greater than one)
    ownSig :: Sig
    ownSig = case findSignatures sigSymbol (txOutValue ownInput) of
      [s] -> s
      _ -> leaveTraceError "Should only have one SIG"

    -- The public key hash from this account owner
    pkh :: PubKeyHash
    pkh = sUser ownSig

    sigValue :: Value
    sigValue = signatureValue' sigSymbol ownSig

    transactionFeeValue :: Value
    transactionFeeValue =
      assetClassValue
        (psToken platformSettings)
        (psTxFee platformSettings)

    -- The validator hash from the platform contracts
    contrValHash :: ValidatorHash
    contrValHash = asContractValidatorHash accountSettings

    -- The UTxO from the contract we are leaving
    contractInput :: TxOut
    contractInput = case strictFindInputWithValHash contrValHash info of
      Just out -> out
      Nothing -> leaveTraceError "No contract being consumed"

    contractNFT :: AssetClass
    contractNFT = case M.lookup contrValHash (adContracts inputDatum) of
      Just ac -> ac
      Nothing -> leaveTraceError "Contract not registered"

    -- Makes sure our account is valid by seeing if the SIG tokens owner signed
    -- this transaction and the validator hash embeded on the SIG token
    -- corresponds to this account's validator hash
    validAccount :: Bool
    validAccount = txSignedBy info pkh && sScript ownSig == ownHash ctx

    -- Makes sure the account received exactly one sig value back and the
    -- corresponding transaction fees
    validAccountValue :: Bool
    validAccountValue =
      txOutValue ownOutput <> negate (txOutValue ownInput)
        == sigValue <> transactionFeeValue

    -- Makes sure the account datum receives the corresponding CAS decrease or
    -- increase and that the contract identifier is removed from the list
    validAccountDatum :: Bool
    validAccountDatum = case returnType of
      ARTLeave ->
        accountDatum
          == leaveContractCAS casMap (removeContract inputDatum contrValHash)
      ARTCancel ->
        accountDatum
          == cancelContractCAS casMap (removeContract inputDatum contrValHash)

    -- Makes sure the contract is valid (contains the NFT identifier) and that
    -- it has been signed by our expelled user
    validContract :: Bool
    validContract = case findContractNFT (txOutValue contractInput) of
      Just ac -> ac == contractNFT
      Nothing -> False

{-# INLINEABLE mkAccountValidator #-}
mkAccountValidator ::
  AccountSettings ->
  AccountDatum ->
  AccountRedeemer ->
  ScriptContext ->
  Bool
mkAccountValidator as dat ACreateContract ctx = validateCreateContract as dat ctx
mkAccountValidator as dat ASign ctx = validateSign as dat ctx
mkAccountValidator as _ (ACollect pkh) ctx = validateCollect as pkh ctx
mkAccountValidator as dat (AReturn t) ctx = validateReturn as t dat ctx

typedAccountValidator :: AccountSettings -> Scripts.TypedValidator AccountType
typedAccountValidator as =
  Scripts.mkTypedValidator @AccountType
    ($$(PlutusTx.compile [||mkAccountValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode as)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @AccountDatum @AccountRedeemer

accountValidator :: AccountSettings -> Validator
accountValidator = Scripts.validatorScript . typedAccountValidator

accountValidatorHash :: AccountSettings -> ValidatorHash
accountValidatorHash = validatorHash . accountValidator

accountAddress :: AccountSettings -> Ledger.Address
accountAddress = scriptAddress . accountValidator
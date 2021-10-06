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

module Membership.OnChain.Contract where

import Ledger
  ( Address,
    POSIXTime,
    PubKeyHash (..),
    ScriptContext (scriptContextTxInfo),
    TxInfo (txInfoValidRange),
    TxOut (txOutValue),
    ValidatorHash,
    contains,
    findDatum,
    from,
    member,
    scriptAddress,
    txSignedBy,
  )
import Ledger.Typed.Scripts as Scripts
  ( TypedValidator,
    Validator,
    mkTypedValidator,
    validatorScript,
    wrapValidator,
  )
import Ledger.Value
  ( AssetClass,
    CurrencySymbol (..),
    TokenName,
    Value,
    assetClass,
    assetClassValue,
    flattenValue,
    geq,
  )
import Membership.Contract
import Membership.Logic
import Membership.OnChain.Utils
import Membership.PlatformSettings
import Membership.Service
import Membership.ShameToken
import Membership.Signature
import Membership.Utils
import qualified PlutusTx
import PlutusTx.AssocMap as Map (delete, lookup)
import PlutusTx.Prelude
  ( Bool (..),
    BuiltinString,
    Eq ((==)),
    Integer,
    Maybe (..),
    Monoid (mempty),
    Semigroup ((<>)),
    elem,
    find,
    fst,
    isJust,
    negate,
    not,
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

{-# INLINEABLE getContractEssentials #-}
getContractEssentials :: BuiltinString -> ContractSettings -> ScriptContext -> ContractEssentials
getContractEssentials validatorName cst ctx =
  ContractEssentials
    { ceInfo = info,
      ceOwnInput = ownInput,
      ceOwnOutput = ownOutput,
      ceOutputDatum = outputDatum,
      ceSigSymbol = sigSymbol,
      ceInputSigs = inputSignatures,
      ceOutputSigs = outputSignatures,
      ceOwnSig = ownSig,
      ceSigValue = sigValue,
      cePresentWhere = presentWhere
    }
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    outputDatum :: ContractDatum
    outputDatum = case findContractDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError (createError validatorName "No Output Datum found")

    sigSymbol :: CurrencySymbol
    sigSymbol = csSignatureSymbol cst

    inputSignatures :: [Sig]
    inputSignatures = findSignatures sigSymbol (txOutValue ownInput)

    outputSignatures :: [Sig]
    outputSignatures = findSignatures sigSymbol (txOutValue ownOutput)

    ownInputSig :: Maybe Sig
    ownInputSig = whoSigned' info inputSignatures

    ownOutputSig :: Maybe Sig
    ownOutputSig = whoSigned' info outputSignatures

    ownSig :: Sig
    ownSig = case (ownInputSig, ownOutputSig) of
      (Just s, _) -> s
      (_, Just s) -> s
      _ -> traceError $ createError validatorName "Sig token was not found"

    -- A value representing a single sig token from our user
    sigValue :: Value
    sigValue = signatureValue sigSymbol (sUser ownSig) (sScript ownSig)

    presentWhere :: PresentWhere
    presentWhere = case (ownInputSig, ownOutputSig) of
      (Just _, Just _) -> PWBoth
      (Just _, Nothing) -> PWInput
      (Nothing, Just _) -> PWOutput
      (Nothing, Nothing) -> traceError $ createError validatorName "Sig token was not found"

{-# INLINEABLE validateContract #-}
validateContract :: ContractSettings -> ScriptContext -> Bool
validateContract contractSettings ctx =
  traceIfFalse
    "Contract - Inconsistent signatures"
    ( consistentSigs outputSignatures
        && consistentSigs inputSignatures
    )
    && traceIfFalse "Contract - NFT is not in both input and output" continuingNFT
  where
    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    sigSymbol :: CurrencySymbol
    sigSymbol = csSignatureSymbol contractSettings

    continuingNFT :: Bool
    continuingNFT = isJust $ do
      inputNFT <- find f (flattenValue $ txOutValue ownInput)
      outputNFT <- find f (flattenValue $ txOutValue ownOutput)
      return (inputNFT, outputNFT)
      where
        f :: (CurrencySymbol, TokenName, Integer) -> Bool
        f (_, tn, amt) = tn == contractNFTTokenName && amt == 1
        f _ = False

    inputSignatures :: [Sig]
    inputSignatures = findSignatures sigSymbol (txOutValue ownInput)

    outputSignatures :: [Sig]
    outputSignatures = findSignatures sigSymbol (txOutValue ownOutput)

{-# INLINEABLE sufficientValue #-}
sufficientValue :: ScriptContext -> Value -> Bool
sufficientValue ctx requiredValue = valueDifference == requiredValue
  where
    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    valueDifference :: Value
    valueDifference = txOutValue ownOutput <> negate (txOutValue ownInput)

{-# INLINEABLE createError #-}
createError :: BuiltinString -> BuiltinString -> BuiltinString
createError validatorName validatorError =
  validatorName
    <> " - "
    <> validatorError

{-# INLINEABLE abstractValidator #-}
abstractValidator :: BuiltinString -> ContractDatum -> Value -> ScriptContext -> Bool
abstractValidator validatorName expectedDatum expectedValueDifference ctx =
  traceIfFalse (createError validatorName "Invalid Datum") validDatum
    && traceIfFalse (createError validatorName "Invalid Value") validValue
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    outputDatum :: ContractDatum
    outputDatum = case findContractDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError (createError validatorName "No Output Datum found")

    validDatum :: Bool
    validDatum = expectedDatum == outputDatum

    validValue :: Bool
    validValue =
      txOutValue ownOutput <> negate (txOutValue ownInput)
        == expectedValueDifference

{-# INLINEABLE signTraceIfFalse #-}
signTraceIfFalse :: BuiltinString -> Bool -> Bool
signTraceIfFalse msg = traceIfFalse ("Contract Sign - " <> msg)

{-# INLINEABLE signTraceError #-}
signTraceError :: forall a. BuiltinString -> a
signTraceError msg = traceError ("Contract Sign - " <> msg)

-- Signing should only validate if
--    * The contract role map increases correctly with
--        * The added user having signed the transaction
--        * If the user is a mediator, he is in the contract list
--        * The user is not a publisher
--    * The necessary value is paid to the contract, being
--        * If the user is a mediator,
--            * The SIG token plus the collateral
--        * If the user is a client
--            * The SIG token plus the collateral plus the price (if it's a one-time contract)
--    * The Datum stays exactly the same (except for the role map)
--    * The Contract is valid
{-# INLINEABLE handleSigning #-}
handleSigning :: ContractSettings -> ContractEssentials -> ContractDatum -> ScriptContext -> Bool
handleSigning cst ce inputDatum ctx =
  validateContract cst ctx
    && signTraceIfFalse "Invalid Contract Value" validValue
    && signTraceIfFalse "Invalid Contract Datum" validDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- The contract input and output UTxOs
    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    -- The data from our contract output
    outputDatum :: ContractDatum
    outputDatum = case findContractDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> signTraceError "No Output Datum found"

    -- The public key hash derived from the SIG token found
    pkh :: PubKeyHash
    pkh = sUser $ ceOwnSig ce

    -- Tries to find our user's role by subtracting the output's role map from the input's one
    userRole :: Role
    userRole = case subtractMaps (cdRoleMap $ ceOutputDatum ce) (cdRoleMap inputDatum) of
      -- If our user is a mediator, he must be in the contract judges list
      Just (pkh', Mediator)
        | pkh' == pkh ->
          if pkh `elem` jsPubKeyHashes (cdJudges inputDatum)
            then Mediator
            else signTraceError "Mediator not in the list"
      Just (pkh', r)
        | pkh' == pkh -> r
      _ -> signTraceError "No role added"

    -- The collateral hold in this contract
    trustValue :: Value
    trustValue = assetClassValue (psToken $ csPlatformSettings cst) (sTrust $ cdService inputDatum)

    -- The price that will be paid to the service provider if everything goes well
    -- If this is a constant contract, there is no price
    priceValue :: Value
    priceValue = case sType $ cdService inputDatum of
      CConstant -> mempty
      OneTime v _ -> v

    -- The value that must result from the subtraction between output and input values
    -- If our user is a client, he must provide his sig token,
    --  his collateral and the price for the service (if it's one-time)
    -- If our user is a mediator, he must only provide his sig value and a collateral
    -- If our user is a publisher, the validation should fail since there can only
    --  be one publisher (the one that created the contract)
    expectedValueDifference :: Value
    expectedValueDifference = case userRole of
      Client -> ceSigValue ce <> trustValue <> priceValue
      Mediator -> ceSigValue ce <> trustValue
      Publisher -> signTraceError "Publisher cannot sign the contract again"

    validValue :: Bool
    validValue =
      txOutValue ownOutput <> negate (txOutValue ownInput)
        == expectedValueDifference

    -- Makes sure our output contract data receives the new user inside it's
    -- role map
    validDatum :: Bool
    validDatum = outputDatum == addUser pkh userRole inputDatum

{-# INLINEABLE accuseTraceIfFalse #-}
accuseTraceIfFalse :: BuiltinString -> Bool -> Bool
accuseTraceIfFalse msg = traceIfFalse ("Contract Accusation - " <> msg)

{-# INLINEABLE accuseTraceError #-}
accuseTraceError :: forall a. BuiltinString -> a
accuseTraceError msg = traceError ("Contract Accusation - " <> msg)

{-# INLINEABLE handleAccusation #-}
handleAccusation ::
  ContractSettings ->
  ContractDatum ->
  Accusation ->
  ScriptContext ->
  Bool
handleAccusation cst inputDatum accusation ctx =
  validateContract cst ctx
    && accuseTraceIfFalse "Invalid Accusation" validAccusation
    && accuseTraceIfFalse "Invalid Contract Value" validContractValue
    && accuseTraceIfFalse "Invalid Contract Datum" validContractDatum
    && accuseTraceIfFalse "Invalid Logic" validLogicScript
    && accuseTraceIfFalse
      "Logic Script did not receive collateral"
      validLogicScriptValue
    && accuseTraceIfFalse "Transaction not signed by accuser" signedByAccuser
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- The contract input and output UTxOs
    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    -- The data from our contract output
    outputDatum :: ContractDatum
    outputDatum = case findContractDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> accuseTraceError "No output datum found"

    signatureSymbol, shameTokenSymbol :: CurrencySymbol
    shameTokenSymbol = psShameTokenSymbol (csPlatformSettings cst)
    signatureSymbol = csSignatureSymbol cst

    -- When we create a contract we must specify the logic validator hash which
    -- will be responsible for handling accusations, mediations and distributing
    -- the trust tokens accordingly. This is the logic we specified
    logicScriptValHash :: ValidatorHash
    logicScriptValHash = cdLogicScript inputDatum

    -- The logic input and output UTxOs
    logicScriptInput, logicScriptOutput :: TxOut
    logicScriptInput = case strictFindInputWithValHash logicScriptValHash info of
      Just o -> o
      Nothing -> accuseTraceError "Couldn't find an unique logic script input"
    logicScriptOutput = case strictFindOutputWithValHash logicScriptValHash info of
      Just o -> o
      Nothing -> accuseTraceError "Couldn't find an unique logic script output"

    -- A digestable shame token representation, with it's essential information
    -- easy to manipulate
    st :: ShameToken
    st = case findShameToken shameTokenSymbol (txOutValue logicScriptInput) of
      Just st' -> st'
      Nothing -> accuseTraceError "Couldn't find a shame token in the logic script input"

    shameTokenAssetClass :: AssetClass
    shameTokenAssetClass = assetClass shameTokenSymbol (stTokenName st)

    -- All the sig tokens which can be found inside our contract input UTxO
    sigs :: [Sig]
    sigs = findSignatures signatureSymbol (txOutValue ownInput)

    accuser, accused :: PubKeyHash
    accuser = fst (aAccuser accusation)
    accused = fst (aAccused accusation)

    accuserRole, accusedRole :: Role
    accuserRole = snd (aAccuser accusation)
    accusedRole = snd (aAccused accusation)

    accusationTime :: POSIXTime
    accusationTime = aTime accusation

    accuserSig, accusedSig, judgeSig :: Sig
    accuserSig = case findSig accuser sigs of
      Just s -> s
      Nothing -> accuseTraceError "Accuser SIG token missing"
    accusedSig = case findSig accused sigs of
      Just s -> s
      Nothing -> accuseTraceError "Accused SIG token missing"
    judgeSig = case firstValidJudge (jsPubKeyHashes $ cdJudges inputDatum) sigs of
      Just s -> sig s (sScript accuserSig)
      Nothing -> accuseTraceError "No available judge"

    accusedSigValue, judgeSigValue :: Value
    accusedSigValue = signatureValue' signatureSymbol accusedSig
    judgeSigValue = signatureValue' signatureSymbol judgeSig

    judgeValue, trustValue, shameTokenValue :: Value
    -- The price a judge will receive after mediating the conflict
    judgeValue =
      assetClassValue
        (psToken $ csPlatformSettings cst)
        (jsPrice $ cdJudges inputDatum)
    -- The value that serves as collateral in case someone breaks the rules
    trustValue =
      assetClassValue
        (psToken $ csPlatformSettings cst)
        (sTrust $ cdService inputDatum)
    -- The value that uniquely identifies the logic script
    shameTokenValue = assetClassValue shameTokenAssetClass 1

    -- The public key hashes from the accuser and accused inside the accusation
    -- we received should have SIG tokens inside this contract
    validAccusation :: Bool
    validAccusation = case (foundAccuserRole, foundAccusedRole) of
      (Just acr, Just acd) ->
        acr == accuserRole
          && acd == accusedRole
          && accusationTime `member` txInfoValidRange info
      _ -> False
      where
        foundAccuserRole, foundAccusedRole :: Maybe Role
        foundAccuserRole = Map.lookup accuser (cdRoleMap inputDatum)
        foundAccusedRole = Map.lookup accused (cdRoleMap inputDatum)

    -- Our contract should lose exatcly the judge and accused trust values and
    -- sig tokens
    validContractValue :: Bool
    validContractValue =
      txOutValue ownOutput <> negate (txOutValue ownInput)
        == negate (trustValue <> trustValue <> accusedSigValue <> judgeSigValue)

    -- If the output Datum matches the expected values
    validContractDatum :: Bool
    validContractDatum = outputDatum == accuseUser accusation inputDatum

    -- If the logic input and output UTxOs contain the shame token value
    validLogicScript :: Bool
    validLogicScript =
      txOutValue logicScriptInput `geq` shameTokenValue
        && txOutValue logicScriptOutput `geq` shameTokenValue

    -- If the logic script received exactly the trust tokens from the judge and
    -- accused, their SIG tokens and the judge expected reward
    validLogicScriptValue :: Bool
    validLogicScriptValue =
      txOutValue logicScriptOutput <> negate (txOutValue logicScriptInput)
        == ( trustValue
               <> trustValue
               <> accusedSigValue
               <> judgeSigValue
               <> judgeValue
           )

    signedByAccuser :: Bool
    signedByAccuser = txSignedBy info accuser

{-# INLINEABLE mediateTraceIfFalse #-}
mediateTraceIfFalse :: BuiltinString -> Bool -> Bool
mediateTraceIfFalse msg = traceIfFalse ("Contract Mediation - " <> msg)

{-# INLINEABLE mediateTraceError #-}
mediateTraceError :: forall a. BuiltinString -> a
mediateTraceError msg = traceError ("Contract Mediation - " <> msg)

{-# INLINEABLE handleMediation #-}
handleMediation ::
  ContractSettings ->
  ContractDatum ->
  ScriptContext ->
  Bool
handleMediation cst inputDatum ctx =
  validateContract cst ctx
    && mediateTraceIfFalse "Invalid Contract" validContract
    && mediateTraceIfFalse "Invalid Logic" validLogic
    && mediateTraceIfFalse "Invalid contract value" validContractValue
    && mediateTraceIfFalse "Invalid contract datum" validContractDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    -- The contract input and output UTxOs
    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    -- The data from our contract output
    outputDatum :: ContractDatum
    outputDatum = case findContractDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> mediateTraceError "No output datum found"

    logicValHash :: ValidatorHash
    logicValHash = cdLogicScript inputDatum

    -- The input UTxO from the logic script we are consuming
    logicInput :: TxOut
    logicInput = case strictFindInputWithValHash logicValHash info of
      Just out -> out
      Nothing -> mediateTraceError "Couldn't find an unique logic script input"

    -- The data / state in which this logic is right now
    -- Because we are trying to complete the mediation (distributing the tokens),
    -- we expect it to be in the LSWaitingEnd state
    logicDatum :: LogicState
    logicDatum = case findLogicDatum logicInput (`findDatum` info) of
      Just (LSWaitingEnd nft jdg acc ver) -> LSWaitingEnd nft jdg acc ver
      Just _ -> mediateTraceError "Wrong logic state"
      Nothing -> mediateTraceError "Logic datum not found"

    -- The NFT used to identify our contract
    contractNFT :: AssetClass
    contractNFT = case logicDatum of
      (LSWaitingEnd nft _ _ _) -> nft

    contractNFTValue :: Value
    contractNFTValue = assetClassValue contractNFT 1

    -- The information about the user who mediated this conflict, such as his
    -- public key hash and when he did it
    judgeInfo :: Judge
    judgeInfo = case logicDatum of
      (LSWaitingEnd _ jdg _ _) -> jdg

    -- The accusation which was judged
    accusation :: Accusation
    accusation = case logicDatum of
      (LSWaitingEnd _ _ acc _) -> acc

    sigSymbol :: CurrencySymbol
    sigSymbol = csSignatureSymbol cst

    judge :: PubKeyHash
    judge = jPubKeyHash judgeInfo

    accused :: PubKeyHash
    accused = fst $ aAccused accusation

    sigs :: [Sig]
    sigs = findSignatures sigSymbol (txOutValue ownOutput)

    -- The validator hash from the platform accounts
    accountValHash :: ValidatorHash
    accountValHash = case sigs of
      (x : _) -> sScript x
      [] -> mediateTraceError "No SIGs found"

    judgeSig, accusedSig :: Sig
    judgeSig = sig judge accountValHash
    accusedSig = sig accused accountValHash

    judgeSigValue, accusedSigValue, trustValue :: Value
    judgeSigValue = signatureValue' sigSymbol judgeSig
    accusedSigValue = signatureValue' sigSymbol accusedSig
    trustValue =
      assetClassValue
        (psToken $ csPlatformSettings cst)
        (sTrust $ cdService inputDatum)

    -- If our service is one-time (therefore has a price), the accuser
    -- is a client and the accused is a publisher, the client should
    -- receive his money back, that's what priceValue accounts for
    priceValue :: Value
    priceValue = case sType (cdService inputDatum) of
      OneTime v _
        | (snd (aAccuser accusation) == Client)
            && (snd (aAccused accusation) == Publisher) ->
          v
      _ -> mempty -- Means "no value"

    -- If our accused was forced to leave the contract
    guilty :: Bool
    guilty
      | ownValueDifference == innocentValue = False
      | ownValueDifference == guiltyValue = True
      | otherwise = mediateTraceError "Invalid contract value"
      where
        ownValueDifference :: Value
        ownValueDifference = txOutValue ownOutput <> negate (txOutValue ownInput)

        innocentValue :: Value
        innocentValue =
          judgeSigValue
            <> accusedSigValue
            <> trustValue
            <> trustValue

        guiltyValue :: Value
        guiltyValue = judgeSigValue <> trustValue <> negate priceValue

    shameTokenAssetClass :: AssetClass
    shameTokenAssetClass = case findShameTokenAssetClass
      (psShameTokenSymbol $ csPlatformSettings cst)
      (txOutValue logicInput) of
      Just ac -> ac
      Nothing -> mediateTraceError "Shame Token could not be found"

    shameTokenValue :: Value
    shameTokenValue = assetClassValue shameTokenAssetClass 1

    sameJudges :: Bool
    sameJudges = cdJudges inputDatum == cdJudges outputDatum

    sameLogicScript :: Bool
    sameLogicScript = cdLogicScript inputDatum == cdLogicScript outputDatum

    sameService :: Bool
    sameService = cdService inputDatum == cdService outputDatum

    -- If our new role map removed the accused (if he was guilty)
    validRoleMap :: Bool
    validRoleMap =
      if guilty
        then cdRoleMap outputDatum == Map.delete accused (cdRoleMap inputDatum)
        else cdRoleMap outputDatum == cdRoleMap inputDatum

    -- Since the accusation has been dealt with, it should be removed from the
    -- contract data
    validAccusation :: Bool
    validAccusation =
      cdAccusations outputDatum
        == cdAccusations (removeAccusation accusation inputDatum)

    -- If our contract contains in both input and output it's NFT
    validContract :: Bool
    validContract =
      txOutValue ownInput `geq` contractNFTValue
        && txOutValue ownOutput `geq` contractNFTValue

    -- The "guilty" variable is already checking that for us
    validContractValue :: Bool
    validContractValue = True

    validLogic :: Bool
    validLogic = txOutValue logicInput `geq` shameTokenValue

    validContractDatum :: Bool
    validContractDatum =
      mediateTraceIfFalse "Datum judges changed" sameJudges
        && mediateTraceIfFalse "Datum logic script changed" sameLogicScript
        && mediateTraceIfFalse "Datum service changed" sameService
        && mediateTraceIfFalse "Invalid datum role map" validRoleMap
        && mediateTraceIfFalse "Invalid datum accusations" validAccusation

{-# INLINEABLE cancelTraceIfFalse #-}
cancelTraceIfFalse :: BuiltinString -> Bool -> Bool
cancelTraceIfFalse msg = traceIfFalse ("Contract Cancel - " <> msg)

{-# INLINEABLE cancelTraceError #-}
cancelTraceError :: forall a. BuiltinString -> a
cancelTraceError msg = traceError ("Contract Cancel - " <> msg)

-- Cancellation should only validate if
--    * The contract is valid
--    * The Datum stays the same except for the role map, which should remove our user from it
--    * The output minus the input values is equal to our sig token (negative)
--    * There is an account in this transaction's input and output
--    * The account is valid
--    * The account receives the SIG token from the user
{-# INLINEABLE handleCancellation #-}
handleCancellation ::
  ContractSettings ->
  ContractDatum ->
  ScriptContext ->
  Bool
handleCancellation cst inputDatum ctx =
  validateContract cst ctx
    && cancelTraceIfFalse "Invalid Account" validAccount
    && cancelTraceIfFalse "Account did not receive SIG token" validAccountValue
    && cancelTraceIfFalse "Invalid Contract Value" validContractValue
    && cancelTraceIfFalse "Invalid Contract Datum" validContractDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    sigSymbol :: CurrencySymbol
    sigSymbol = csSignatureSymbol cst

    -- The contract input and output UTxOs
    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    -- The data from our contract output
    outputDatum :: ContractDatum
    outputDatum = case findContractDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> cancelTraceError "No Output Datum found"

    -- All the SIG wrappers inside our contract
    sigs :: [Sig]
    sigs = findSignatures sigSymbol (txOutValue ownInput)

    -- The SIG wrapper from the person who signed this (the one who wants to
    -- cancel the contract)
    ownSig :: Sig
    ownSig = case whoSigned' info sigs of
      Just s -> s
      Nothing -> cancelTraceError "Sig not found"

    pkh :: PubKeyHash
    pkh = sUser ownSig

    accValHash :: ValidatorHash
    accValHash = sScript ownSig

    sigValue :: Value
    sigValue = signatureValue' sigSymbol ownSig

    -- The account input and output UTxOs from the user who want's to cancel
    -- the contract
    accountInput, accountOutput :: TxOut
    accountInput = case strictFindInputWithValHash accValHash info of
      Just o -> o
      Nothing -> cancelTraceError "Couldn't find an unique account input"
    accountOutput = case strictFindOutputWithValHash accValHash info of
      Just o -> o
      Nothing -> cancelTraceError "Couldn't find an unique account output"

    -- Make sure the account input and output has our user's SIG token (it's
    -- actually his account)
    validAccount :: Bool
    validAccount =
      txOutValue accountInput `geq` sigValue
        && txOutValue accountOutput `geq` sigValue

    -- Make sure the account receives our user SIG token back
    validAccountValue :: Bool
    validAccountValue =
      txOutValue accountOutput <> negate (txOutValue accountInput)
        == sigValue

    -- Ensure our contract datum had the user removed from the role map
    validContractDatum :: Bool
    validContractDatum = outputDatum == removeUser pkh inputDatum

    -- Make sure our contract lost exactly one SIG token (from the person who
    -- signed this transaction)
    validContractValue :: Bool
    validContractValue =
      txOutValue ownOutput <> negate (txOutValue ownInput)
        == negate sigValue

{-# INLINEABLE leaveTraceIfFalse #-}
leaveTraceIfFalse :: BuiltinString -> Bool -> Bool
leaveTraceIfFalse msg = traceIfFalse ("Contract Leave - " <> msg)

{-# INLINEABLE leaveTraceError #-}
leaveTraceError :: forall a. BuiltinString -> a
leaveTraceError msg = traceError ("Contract Leave - " <> msg)

{-# INLINEABLE handleLeave #-}
handleLeave ::
  ContractSettings ->
  ContractDatum ->
  ScriptContext ->
  Bool
handleLeave cst inputDatum ctx =
  validateContract cst ctx
    && leaveTraceIfFalse "Invalid Contract Value" validContractValue
    && leaveTraceIfFalse "Invalid Contract Datum" validContractDatum
    && leaveTraceIfFalse "Invalid account" validAccount
    && leaveTraceIfFalse "Account did not receive SIG token" validAccountValue
    && leaveTraceIfFalse "Not allowed to leave the contract now" allowedToLeave
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    sigSymbol :: CurrencySymbol
    sigSymbol = csSignatureSymbol cst

    platformSettings :: PlatformSettings
    platformSettings = csPlatformSettings cst

    -- The contract input and output UTxOs
    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    -- The data from our contract output
    outputDatum :: ContractDatum
    outputDatum = case findContractDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> leaveTraceError "No Output Datum found"

    -- All the SIG wrappers inside our contract
    sigs :: [Sig]
    sigs = findSignatures sigSymbol (txOutValue ownInput)

    -- The SIG wrapper from the user who signed this (the one who wants to
    -- leave the contract)
    ownSig :: Sig
    ownSig = case whoSigned' info sigs of
      Just s -> s
      Nothing -> leaveTraceError "Sig not found"

    pkh :: PubKeyHash
    pkh = sUser ownSig

    -- The platform account validator hash, derived from the user SIG token
    accValHash :: ValidatorHash
    accValHash = sScript ownSig

    -- The current role from the user who want's to leave
    userRole :: Role
    userRole = case Map.lookup pkh (cdRoleMap inputDatum) of
      Just r -> r
      Nothing -> leaveTraceError "User not registered"

    -- The account input and output UTxOs from the user who want's to leave
    -- the contract
    accountInput, accountOutput :: TxOut
    accountInput = case strictFindInputWithValHash accValHash info of
      Just o -> o
      Nothing -> leaveTraceError "Couldn't find an unique account input"
    accountOutput = case strictFindOutputWithValHash accValHash info of
      Just o -> o
      Nothing -> leaveTraceError "Couldn't find an unique account output"

    trustValue, priceValue, sigValue :: Value

    -- The collateral held in this contract
    trustValue = assetClassValue (psToken $ csPlatformSettings cst) (sTrust $ cdService inputDatum)

    -- The price of the service (if we are the publisher and the contract is of
    -- type one-time)
    priceValue = case sType $ cdService inputDatum of
      OneTime v _
        | userRole == Publisher -> v
      _ -> mempty

    sigValue = signatureValue' sigSymbol ownSig

    transactionFeeValue =
      assetClassValue
        (psToken platformSettings)
        (psTxFee platformSettings)

    -- Did the publisher leave the contract already?
    publisherPresent :: Bool
    publisherPresent = sigIn (sPublisher $ cdService inputDatum) sigs

    -- Makes sure our contract lost exactly the value of the user trust, his SIG
    -- token and the price (if he's a publisher)
    validContractValue :: Bool
    validContractValue =
      txOutValue ownOutput <> negate (txOutValue ownInput)
        == negate (trustValue <> sigValue <> priceValue)

    -- The new contract datum must have the user removed from the role map
    validContractDatum :: Bool
    validContractDatum = outputDatum == removeUser pkh inputDatum

    -- Makes sure the account has our user's sig value in both input and output
    validAccount :: Bool
    validAccount =
      txOutValue accountInput `geq` sigValue
        && txOutValue accountOutput `geq` sigValue

    -- Makes sure the account received exactly our user's SIG value and the
    -- expected transaction fee
    validAccountValue :: Bool
    validAccountValue =
      txOutValue accountOutput <> negate (txOutValue accountInput)
        == sigValue <> transactionFeeValue

    accusations :: [Accusation]
    accusations = cdAccusations inputDatum

    -- Makes sure our user is actually allowed to leave
    -- In any type, we must always verify that our user is not involved in any
    -- accusation (this includes he being a mediator)
    -- If our contract is constant that's it, but if our contract is one-time,
    -- we must also, either the deadline must have passed or the publisher
    -- cancelled the contract already
    allowedToLeave :: Bool
    allowedToLeave =
      not (involvedInAccusation pkh accusations)
        && case sType $ cdService inputDatum of
          OneTime _ dln ->
            (from dln `contains` txInfoValidRange info) || not publisherPresent
          CConstant -> True

{-# INLINEABLE handleReview #-}
handleReview ::
  ContractSettings ->
  Review ->
  PubKeyHash ->
  ContractDatum ->
  ScriptContext ->
  Bool
handleReview _ _ _ _ _ = traceError "Contract Review - Incomplete"

{-# INLINEABLE mkContractValidator #-}
mkContractValidator :: ContractSettings -> ContractDatum -> ContractRedeemer -> ScriptContext -> Bool
mkContractValidator cst dat CSign ctx = handleSigning cst ce dat ctx
  where
    ce :: ContractEssentials
    ce = getContractEssentials "Contract Signature" cst ctx
mkContractValidator cst dat (CAccuse acc) ctx = handleAccusation cst dat acc ctx
mkContractValidator cst dat CMediate ctx = handleMediation cst dat ctx
mkContractValidator cst dat CCancel ctx = handleCancellation cst dat ctx
mkContractValidator cst dat CLeave ctx = handleLeave cst dat ctx

typedContractValidator :: ContractSettings -> Scripts.TypedValidator ContractType
typedContractValidator ps =
  Scripts.mkTypedValidator @ContractType
    ($$(PlutusTx.compile [||mkContractValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode ps)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @ContractDatum @ContractRedeemer

contractValidator :: ContractSettings -> Validator
contractValidator = Scripts.validatorScript . typedContractValidator

contractAddress :: ContractSettings -> Ledger.Address
contractAddress = scriptAddress . contractValidator
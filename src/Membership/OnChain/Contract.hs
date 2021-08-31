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
    POSIXTime (POSIXTime),
    PubKeyHash,
    ScriptContext (scriptContextTxInfo),
    TxInfo (txInfoValidRange),
    TxOut (txOutValue),
    ValidatorHash,
    contains,
    findDatum,
    from,
    member,
    ownHash,
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
    Value,
    assetClass,
    assetClassValue,
    geq,
  )
import Membership.Contract
  ( Accusation (Accusation),
    ContractDatum
      ( cdAccusations,
        cdInputs,
        cdJudges,
        cdLogicScript,
        cdRoleMap,
        cdService
      ),
    ContractRedeemer (CAccuse, CCollect, CSign),
    ContractType,
    Judges (Judges),
    Role (..),
    currentJudge,
    findContractDatum,
  )
import Membership.OnChain.Utils (sigTokenIn, strictFindOutAndIn)
import Membership.PlatformSettings
  ( ContractSettings (csPlatformToken, csSignatureSymbol),
  )
import Membership.Service
  ( Service (sTrust, sType),
    ServiceType (CConstant, OneTime),
  )
import Membership.Signature
  ( Sig (sScript, sUser),
    findSignatures,
    makeSigToken,
    signatureAssetClass,
    signatureValue,
  )
import Membership.Utils (subtractMaps)
import qualified PlutusTx
import PlutusTx.AssocMap as Map (delete, insert)
import PlutusTx.Prelude
  ( AdditiveSemigroup ((+)),
    Bool (..),
    Eq ((==)),
    Maybe (Just, Nothing),
    Monoid (mempty),
    Semigroup ((<>)),
    any,
    find,
    fst,
    isJust,
    negate,
    not,
    snd,
    traceError,
    traceIfFalse,
    ($),
    (&&),
    (.),
    (||),
  )

{-# INLINEABLE handleSigning #-}
handleSigning :: ContractSettings -> ContractDatum -> ScriptContext -> Bool
handleSigning cst inputDatum ctx =
  traceIfFalse "insufficient value" sufficientValue
    && traceIfFalse "transaction not signed by sig token" (txSignedBy info pkh)
    && traceIfFalse "corrupted datum" rightDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    outputDatum :: ContractDatum
    outputDatum = case findContractDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "no output datum found"

    pkh :: PubKeyHash
    pkh = case subtractMaps (cdRoleMap outputDatum) (cdRoleMap inputDatum) of
      Just (p, _) -> p
      _ -> traceError "No role added"

    userRole :: Role
    userRole = case subtractMaps (cdRoleMap outputDatum) (cdRoleMap inputDatum) of
      Just (_, r) -> r
      _ -> traceError "No role added"

    sig :: Sig
    sig = case find (\s -> sUser s == pkh) (findSignatures (csSignatureSymbol cst) (txOutValue ownOutput)) of
      Just s -> s
      Nothing -> traceError "Sign - SIG token missing 1"

    accountVH :: ValidatorHash
    accountVH = sScript sig

    sigValue :: Value
    sigValue = signatureValue (csSignatureSymbol cst) pkh accountVH

    trust :: Value
    trust = assetClassValue (csPlatformToken cst) (sTrust $ cdService inputDatum)

    price :: Value
    price = case sType $ cdService inputDatum of
      CConstant -> mempty
      OneTime v _ -> v

    sufficientValue :: Bool
    sufficientValue =
      (txOutValue ownOutput <> negate (txOutValue ownInput)) `geq` requiredValue
      where
        requiredValue :: Value
        requiredValue = case userRole of
          Client -> sigValue <> trust <> price
          Mediator -> sigValue <> trust
          Publisher -> traceError "publisher cannot sign the contract again"

    rightDatum :: Bool
    rightDatum =
      cdJudges inputDatum == cdJudges outputDatum
        && cdInputs inputDatum == cdInputs outputDatum
        && cdLogicScript inputDatum == cdLogicScript outputDatum
        && cdAccusations inputDatum == cdAccusations outputDatum
        && cdService inputDatum == cdService outputDatum
        && Map.insert pkh userRole (cdRoleMap inputDatum) == cdRoleMap outputDatum

{-# INLINEABLE handleAccusation #-}
handleAccusation ::
  ContractSettings ->
  ContractDatum ->
  (PubKeyHash, PubKeyHash) ->
  ScriptContext ->
  Bool
handleAccusation cst inputDatum (accuser, accused) ctx =
  traceIfFalse "insufficient value" sufficientValue
    && traceIfFalse "Accuser SIG token missing" (isJust accuserSig)
    && traceIfFalse "Accused SIG token missing" (isJust accusedSig)
    && traceIfFalse "transaction not signed by accuser" (txSignedBy info accuser)
    && traceIfFalse "corrupted datum" rightDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    outputDatum :: ContractDatum
    outputDatum = case findContractDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "no output datum found"

    sigs :: [Sig]
    sigs = findSignatures (csSignatureSymbol cst) (txOutValue ownOutput)

    accuserSig :: Maybe Sig
    accuserSig = find (\s -> sUser s == accuser) sigs

    accusedSig :: Maybe Sig
    accusedSig = find (\s -> sUser s == accused) sigs

    judgePrice :: Value
    judgePrice = case cdJudges inputDatum of
      (Judges _ price _) -> assetClassValue (csPlatformToken cst) price

    sufficientValue :: Bool
    sufficientValue =
      (txOutValue ownOutput <> negate (txOutValue ownInput)) == requiredValue
      where
        requiredValue :: Value
        requiredValue = judgePrice

    sameJudges :: Bool
    sameJudges = cdJudges inputDatum == cdJudges outputDatum

    sameInputs :: Bool
    sameInputs = cdInputs inputDatum == cdInputs outputDatum

    sameLogicScript :: Bool
    sameLogicScript = cdLogicScript inputDatum == cdLogicScript outputDatum

    sameService :: Bool
    sameService = cdService inputDatum == cdService outputDatum

    sameRoleMap :: Bool
    sameRoleMap = cdRoleMap inputDatum == cdRoleMap outputDatum

    rightDatum :: Bool
    rightDatum =
      traceIfFalse "datum judges are not the same" sameJudges
        && traceIfFalse "datum inputs are not the same" sameInputs
        && traceIfFalse "datum logic scripts are not the same" sameLogicScript
        && traceIfFalse "datum services are not the same" sameService
        && traceIfFalse "datum role maps are not the same" sameRoleMap
        && traceIfFalse
          "datum accusation was not added correctly"
          ( case cdAccusations outputDatum of
              ((Accusation accuser' accused' accusationTime) : accusations) ->
                accusationTime `member` txInfoValidRange info
                  && accuser == accuser'
                  && accused == accused'
                  && accusations == cdAccusations inputDatum
              _ -> False
          )

{-# INLINEABLE handleCollect #-}
handleCollect ::
  ContractSettings ->
  ContractDatum ->
  ScriptContext ->
  Bool
handleCollect cst inputDatum ctx =
  traceIfFalse "wrong output value" rightValue
    && traceIfFalse "SIG token missing" (sigTokenIn sigToken ctx)
    && traceIfFalse "transaction not signed by sig token" (txSignedBy info pkh)
    && traceIfFalse "corrupted datum" rightDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput, ownOutput :: TxOut
    ownInput = fst $ strictFindOutAndIn ctx
    ownOutput = snd $ strictFindOutAndIn ctx

    outputDatum :: ContractDatum
    outputDatum = case findContractDatum ownOutput (`findDatum` info) of
      Just dat -> dat
      Nothing -> traceError "no output datum found"

    valueDifference :: Value
    valueDifference = txOutValue ownOutput <> negate (txOutValue ownInput)

    pkh :: PubKeyHash
    pkh = case subtractMaps (cdRoleMap outputDatum) (cdRoleMap inputDatum) of
      Just (p, _) -> p
      _ -> traceError "No role added"

    userRole :: Role
    userRole = case subtractMaps (cdRoleMap inputDatum) (cdRoleMap outputDatum) of
      Just (_, r) -> r
      _ -> traceError "No role added"

    sig :: Sig
    sig = case find (\s -> sUser s == pkh) (findSignatures (csSignatureSymbol cst) (txOutValue ownOutput)) of
      Just s -> s
      Nothing -> traceError "SIG token missing"

    accountVH :: ValidatorHash
    accountVH = sScript sig

    sigToken :: AssetClass
    sigToken = signatureAssetClass (csSignatureSymbol cst) pkh accountVH

    sigValue :: Value
    sigValue = signatureValue (csSignatureSymbol cst) pkh accountVH

    service :: Service
    service = cdService inputDatum

    trust :: Value
    trust = assetClassValue (csPlatformToken cst) (sTrust service)

    -- In the actual application, this should be inside PlatformSettings
    timeToVerifyService :: POSIXTime
    timeToVerifyService = POSIXTime 0

    beforeDeadline :: POSIXTime -> Bool
    beforeDeadline dln =
      not $
        from (dln + timeToVerifyService) `contains` txInfoValidRange info

    price :: Value
    price = case (sType service, userRole) of
      (OneTime p d, Publisher)
        | beforeDeadline d -> p
      _ -> mempty

    involvedInAccusation :: Bool
    involvedInAccusation = case userRole of
      Mediator ->
        any
          ( \acc ->
              isJust $
                currentJudge
                  (csSignatureSymbol cst)
                  acc
                  (txInfoValidRange info)
                  (inputDatum, txOutValue ownInput)
          )
          (cdAccusations inputDatum)
      _ ->
        any
          (\(Accusation acd acr _) -> pkh == acd || pkh == acr)
          (cdAccusations inputDatum)

    -- If a script is trying to consume the contract UTxO, it shall only be
    -- allowed if
    --   - It corresponds to the logic script
    --   - The party that loses trust tokens is removed from the rolesMap
    --   - Judges are rewarded accordingly
    --   - Accusations list is shrinked, removing the accusation
    --   - CAS score increases or decreases accordingly

    rightValue :: Bool
    rightValue = valueDifference == requiredValue
      where
        requiredValue :: Value
        requiredValue =
          if involvedInAccusation
            then negate sigValue
            else negate (sigValue <> trust <> price)

    rightDatum :: Bool
    rightDatum =
      cdJudges inputDatum == cdJudges outputDatum
        && cdInputs inputDatum == cdInputs outputDatum
        && cdLogicScript inputDatum == cdLogicScript outputDatum
        && cdAccusations inputDatum == cdAccusations outputDatum
        && cdService inputDatum == cdService outputDatum
        && cdRoleMap inputDatum == Map.delete pkh (cdRoleMap outputDatum)

{-# INLINEABLE mkContractValidator #-}
mkContractValidator :: ContractSettings -> ContractDatum -> ContractRedeemer -> ScriptContext -> Bool
mkContractValidator cst dat CSign ctx = handleSigning cst dat ctx
mkContractValidator cst dat (CAccuse acr acd) ctx = handleAccusation cst dat (acr, acd) ctx
mkContractValidator cst dat CCollect ctx = handleCollect cst dat ctx
mkContractValidator _ _ _ _ = False

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
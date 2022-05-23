{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import Cardano.Api hiding (Value, TxOut, Script)
import Cardano.Api.Shelley hiding (Value, TxOut, Script)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import Data.Aeson hiding (Value)
import GHC.Generics
import Ledger hiding (singleton)
import Ledger.Typed.Scripts as Scripts hiding (validatorHash)
import Ledger.Value
import Codec.Serialise hiding (encode)
import qualified Prelude
import qualified PlutusTx
import qualified PlutusTx.AssocMap as PlutusMap
import qualified Plutus.V1.Ledger.Api as Plutus
import PlutusTx (Data (..))
import PlutusTx.Prelude
import Plutus.Contracts.MultiSig (validate)

-- This member can receive any of these rewards
-- type Distribution = PlutusMap.Map Member Reward

-- Action is what can be executed by the users via a request, or execution or fallback

-- Action can be either a distribtuion, closing the contract and distributing it's value
-- or StatusQuo which simply means the contract stays as it is

data Action = Distribution [(Member, Reward)] | StatusQuo
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Action where
  {-# INLINEABLE (==) #-}
  Distribution m == Distribution m' = m == m'
  StatusQuo == StatusQuo = True
  _ == _ = False


-- Defines who can execute any of these actions
type DistributionMap = [(Member, [Action])]

type RequesterMap = [(Member, DistributionMap)]

-- A member can be either a specific user or a role

-- Specific users don't need to pay collateral. If you want a user to pay collateral,
-- you must create a role just for him.

-- Roles are groups of users that receive a token once they pay the collateral used
-- to prove they are part of this contract

data Member = User PubKeyHash | Role Integer
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Member where
  {-# INLINEABLE (==) #-}
  User usr == User usr' = usr == usr'
  Role r == Role r' = r == r'
  _ == _ = False


-- Reward is what each user will receive in a distribution. It can be an exact
-- value or a weight.

-- Weight is determined dividing the weight by the total number
-- of users from all roles and users part of the distribution

data Reward = Weight Integer | Amount Value
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq Reward where
  {-# INLINEABLE (==) #-}
  Weight w == Weight w' = w == w'
  Amount val == Amount val' = val == val'
  _ == _ = False

type Collateral = Value
type TotalMembers = Integer

data ContractDatum = ContractDatum
  { cdCreatePolicy :: CurrencySymbol,
    cdRules :: BuiltinByteString,
    cdRoleMap :: [(Collateral, TotalMembers)], -- Role is the index of the list
    cdRequests :: [(Member, [(Member, [Action])])],
    cdExecutions :: [(Member, [Action])], -- Can only be done when frozen is false
    cdFallbacks :: [(Member, [Action])], -- Can be done at any time
    cdFrozen :: Bool
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq ContractDatum where
  {-# INLINEABLE (==) #-}
  ContractDatum nft rls rm rq ex fb fz 
    == ContractDatum nft' rls' rm' rq' ex' fb' fz' =
      nft == nft' && rls == rls' && rm == rm' && rq == rq'
        && ex == ex' && fb == fb' && fz == fz'

{-# INLINEABLE findContractDatum #-}
findContractDatum :: DatumHash -> (DatumHash -> Maybe Datum) -> Maybe ContractDatum
findContractDatum dh f = do
  Datum d <- f dh
  PlutusTx.fromBuiltinData d

-- Contract Validator - Basically delegates validatiton
-- responsibility to the minting policies

{-# INLINEABLE mkContractValidator #-}
mkContractValidator ::
  [CurrencySymbol] ->
  ContractDatum ->
  CurrencySymbol ->
  ScriptContext ->
  Bool
mkContractValidator alwTkts _ tkt ctx = validTicket && ticketPresent
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    -- By ticket tkt I mean a minting policy, we are delegating our
    -- validation to multiple possible scripts

    -- Make sure the ticket we are validating is one of the allowed ones
    validTicket :: Bool
    validTicket = tkt `elem` alwTkts

    -- We simply valdiate that the minting policy is being used
    -- We don't care about how many tokens are being minted or burnt
    ticketPresent :: Bool
    ticketPresent = 
        any 
            (\(cs,_,_) -> cs == tkt)
            (flattenValue (txInfoMint txInfo))

{-# INLINABLE mkAlwaysValidatePolicy #-}
mkAlwaysValidatePolicy :: () -> ScriptContext -> Bool
mkAlwaysValidatePolicy _ _ = True

-- This should not be one of the tickets, since it doesn't validate
-- the script spending, but rather the contract creation
{-# INLINEABLE mkCreatePolicy #-}
mkCreatePolicy ::
  TxOutRef ->
  ValidatorHash ->
  ScriptContext ->
  Bool
mkCreatePolicy oref vh ctx =
    hasUTxO && validContractValue && validContractDatum && validMintedValue
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    contractOutput :: TxOut 
    contractOutput =  case filter predicate (txInfoOutputs info) of
        [o] -> o
      where
        predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

    contractDatum :: ContractDatum 
    contractDatum = case (do
        dh <- txOutDatumHash contractOutput
        findContractDatum dh (`findDatum` info)) of
      Just dat -> dat

    -- Make sure we are consuming the referenced utxo
    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    contractNFT :: Value
    contractNFT = singleton (ownCurrencySymbol ctx) "" 1

    validContractValue :: Bool
    validContractValue =
      txOutValue contractOutput
        `geq` (contractNFT -- Need to multiply value by total members
          <> foldr (\cur acc -> acc <> fst cur `multiplyValue` snd cur) mempty (cdRoleMap contractDatum))
        where
          multiplyValue :: Value -> Integer -> Value
          multiplyValue _ 0 = mempty
          multiplyValue val 1 = val
          multiplyValue val n = val <> multiplyValue val (n-1) 

    validContractDatum :: Bool
    validContractDatum = cdCreatePolicy contractDatum == ownCurrencySymbol ctx

    validMintedValue :: Bool
    validMintedValue =
      valueOf (txInfoMint info) (ownCurrencySymbol ctx) "" == 1
        && all (\(i, (col, mbs)) ->
            valueOf
              (txInfoMint info)
              (ownCurrencySymbol ctx)
              (TokenName $ integerToByteString i) == mbs
           ) (zip [0,1..] (cdRoleMap contractDatum))

    integerToByteString :: Integer -> BuiltinByteString
    integerToByteString n
      | n == 0 = "0"
      | n == 1 = "1"
      | n == 2 = "2"
      | n == 3 = "3"
      | n == 4 = "4"
      | n == 5 = "5"
      | n == 6 = "6"
      | n == 7 = "7"
      | n == 8 = "8"
      | n == 9 = "9"
      | otherwise = integerToByteString (n `divide` 10) <> integerToByteString (n `modulo` 10)


-- Sign and create should be same policy

{-# INLINABLE mkSignPolicy #-}
mkSignPolicy :: (ValidatorHash, [(Integer, Integer)]) -> ScriptContext -> Bool
mkSignPolicy (vh, roles) ctx = validContractValue && validContractDatum
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    input :: TxOut
    input = case filter predicate ((map txInInfoResolved . txInfoInputs) info) of
        [o] -> o
      where
        predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

    output :: TxOut 
    output =  case filter predicate (txInfoOutputs info) of
        [o] -> o
      where
        predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

    inputDatum :: ContractDatum 
    inputDatum = case (do
        dh <- txOutDatumHash input
        findContractDatum dh (`findDatum` info)) of
      Just dat -> dat

    outputDatum :: ContractDatum 
    outputDatum = case (do
        dh <- txOutDatumHash output
        findContractDatum dh (`findDatum` info)) of
      Just dat -> dat

    validContractValue :: Bool
    validContractValue =
      txOutValue output
        == txOutValue input
          <> foldr (\cur acc -> acc <> fst (cdRoleMap inputDatum !! fst cur) `multiplyValue` snd cur) mempty roles
        where
          multiplyValue :: Value -> Integer -> Value
          multiplyValue _ 0 = mempty
          multiplyValue val 1 = val
          multiplyValue val n = val <> multiplyValue val (n-1) 

    validContractDatum :: Bool
    validContractDatum =
      outputDatum == ContractDatum
        { cdCreatePolicy = cdCreatePolicy inputDatum,
          cdRules = cdRules inputDatum,
          cdRoleMap = addMembers (cdRoleMap inputDatum) roles,
          cdRequests = cdRequests inputDatum,
          cdExecutions = cdExecutions inputDatum,
          cdFallbacks = cdFallbacks inputDatum,
          cdFrozen = cdFrozen inputDatum 
        }
    

addMembers :: [(Value, Integer)] -> [(Integer, Integer)] -> [(Value, Integer)]
addMembers roleMap [] = roleMap
addMembers roleMap ((idx, mbs) : xs) =
  addMembers (
    map
      (\(idx', (col, mbs')) -> if idx' == idx then (col, mbs) else (col, mbs'))
      (zip [0,1..] roleMap)
  ) xs

-- addMembers :: ContractDatum -> [(Integer, Integer)] -> ContractDatum
-- addMembers dat ((idx, mbs) : xs) = ContractDatum
--   { cdCreatePolicy = cdCreatePolicy dat,
--     cdRules = cdRules dat,
--     cdRoleMap = map (\(idx', (col, mbs')) -> if idx' == idx then (col, mbs) else (col, mbs')) (zip [0,1..] (cdRoleMap dat)),
--     cdRequests = cdRequests dat,
--     cdExecutions = cdExecutions dat,
--     cdFallbacks = cdFallbacks dat,
--     cdFrozen = cdFrozen dat 
--   } 

data ContractType

instance Scripts.ValidatorTypes ContractType where
  type DatumType ContractType = ContractDatum
  type RedeemerType ContractType = CurrencySymbol

typedContractValidator :: [CurrencySymbol] -> Scripts.TypedValidator ContractType
typedContractValidator tkts =
  Scripts.mkTypedValidator @ContractType
    ( $$(PlutusTx.compile [||mkContractValidator||])
        `PlutusTx.applyCode` PlutusTx.liftCode tkts
    )
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @ContractDatum @CurrencySymbol

contractValidator :: [CurrencySymbol] -> Validator
contractValidator = Scripts.validatorScript . typedContractValidator

contractValidatorHash :: [CurrencySymbol] -> ValidatorHash
contractValidatorHash = validatorHash . contractValidator

contractAddress :: [CurrencySymbol] -> Ledger.Address
contractAddress = scriptAddress . contractValidator

alwaysValidatePolicy :: Scripts.MintingPolicy
alwaysValidatePolicy =
  mkMintingPolicyScript
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy mkAlwaysValidatePolicy||])

alwaysValidateSymbol :: CurrencySymbol
alwaysValidateSymbol = scriptCurrencySymbol alwaysValidatePolicy

alwaysValidateScript :: Script
alwaysValidateScript = unMintingPolicyScript alwaysValidatePolicy

alwaysValidateValidator :: Validator
alwaysValidateValidator = Validator alwaysValidateScript

createPolicy :: TxOutRef -> Scripts.MintingPolicy
createPolicy oref =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||]) `PlutusTx.applyCode` PlutusTx.liftCode oref
  where 
    wrap oref' = Scripts.wrapMintingPolicy $ mkCreatePolicy oref'

createSymbol :: TxOutRef -> CurrencySymbol
createSymbol = scriptCurrencySymbol . createPolicy 

createScript :: TxOutRef -> Script
createScript = unMintingPolicyScript . createPolicy

createValidator :: TxOutRef -> Validator
createValidator = Validator . createScript

PlutusTx.makeLift ''Member
PlutusTx.makeIsDataIndexed ''Member [('User,0), ('Role,1)]

PlutusTx.makeLift ''Reward
PlutusTx.makeIsDataIndexed ''Reward [('Weight,0), ('Amount,1)]

PlutusTx.makeLift ''Action
PlutusTx.makeIsDataIndexed ''Action [('Distribution,0),('StatusQuo,1)]

PlutusTx.makeLift ''ContractDatum
PlutusTx.makeIsDataIndexed ''ContractDatum [('ContractDatum,0)]

-- Serialization

contractValidatorSerialized :: [CurrencySymbol] -> B.ByteString
contractValidatorSerialized tkts = B16.encode $ serialiseToCBOR ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Plutus.unValidatorScript $ contractValidator tkts) :: PlutusScript PlutusScriptV1)

alwaysValidateSerialized :: B.ByteString
alwaysValidateSerialized = B16.encode $ serialiseToCBOR ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Plutus.unValidatorScript alwaysValidateValidator) :: PlutusScript PlutusScriptV1)

createSerialized :: TxOutRef -> B.ByteString
createSerialized oref = B16.encode $ serialiseToCBOR ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Plutus.unValidatorScript $ createValidator oref) :: PlutusScript PlutusScriptV1)

-- Datum

sampleContractDatum :: ContractDatum
sampleContractDatum = ContractDatum
  { cdCreatePolicy = alwaysValidateSymbol
  , cdRules = "bbb"
  , cdRoleMap = [(mempty, 0)]
  , cdRequests = [(Role 0, [(Role 1, [StatusQuo])])]
  , cdExecutions = [(Role 1, [StatusQuo])]
  , cdFallbacks = [(Role 1, [StatusQuo])]
  , cdFrozen = True
  }

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => Prelude.FilePath -> a -> Prelude.IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

-- main

tickets :: [CurrencySymbol]
tickets = [alwaysValidateSymbol] 

main :: Prelude.IO ()
main = do
  Prelude.putStrLn $ "ContractValidator:\n" ++ Prelude.show (contractValidatorSerialized tickets)
  Prelude.putStrLn $ "Always Validate:\n" ++ Prelude.show alwaysValidateSerialized
  writeJSON "./contract.json" sampleContractDatum


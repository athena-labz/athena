{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
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

data ActionRedeemer = Execution Integer Integer
                    | Fallback Integer Integer
                    | Request Integer Integer Integer
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

{-# INLINABLE addMember #-}
addMember :: [(Value, Integer)] -> Integer -> [(Value, Integer)]
addMember roleMap idx = map (\(i, (val, mbs)) -> if i == idx then (val, mbs+1) else (val, mbs)) (zip [0,1..] roleMap)

{-# INLINABLE integerToByteString #-}
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
mkContractValidator alwTkts _ tkt ctx = True

-- {-# INLINEABLE mkContractValidator #-}
-- mkContractValidator ::
--   [CurrencySymbol] ->
--   ContractDatum ->
--   CurrencySymbol ->
--   ScriptContext ->
--   Bool
-- mkContractValidator alwTkts _ tkt ctx = validTicket && ticketPresent
--   where
--     txInfo :: TxInfo
--     txInfo = scriptContextTxInfo ctx

--     -- By ticket tkt I mean a minting policy, we are delegating our
--     -- validation to multiple possible scripts

--     -- Make sure the ticket we are validating is one of the allowed ones
--     validTicket :: Bool
--     validTicket = tkt `elem` alwTkts

--     -- We simply valdiate that the minting policy is being used
--     -- We don't care about how many tokens are being minted or burnt
--     ticketPresent :: Bool
--     ticketPresent = 
--         any 
--             (\(cs,_,_) -> cs == tkt)
--             (flattenValue (txInfoMint txInfo))

{-# INLINEABLE mkCreatePolicy #-}
mkCreatePolicy ::
  TxOutRef ->
  ValidatorHash ->
  ScriptContext ->
  Bool
mkCreatePolicy oref vh ctx = True

-- Should create and sign policies be the same thing?

-- This should not be one of the tickets, since it doesn't validate
-- the script spending, but rather the contract creation
-- {-# INLINEABLE mkCreatePolicy #-}
-- mkCreatePolicy ::
--   TxOutRef ->
--   ValidatorHash ->
--   ScriptContext ->
--   Bool
-- mkCreatePolicy oref vh ctx =
--     hasUTxO && validContractValue && validContractDatum && validMintedValue
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx

--     contractOutput :: TxOut 
--     contractOutput =  case filter predicate (txInfoOutputs info) of
--         [o] -> o
--       where
--         predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

--     contractDatum :: ContractDatum 
--     contractDatum = case (do
--         dh <- txOutDatumHash contractOutput
--         findContractDatum dh (`findDatum` info)) of
--       Just dat -> dat

--     -- Make sure we are consuming the referenced utxo
--     hasUTxO :: Bool
--     hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

--     contractNFT :: Value
--     contractNFT = singleton (ownCurrencySymbol ctx) "" 1

--     validContractValue :: Bool
--     validContractValue =
--       txOutValue contractOutput
--         `geq` (contractNFT -- Need to multiply value by total members
--           <> foldr
--                 (\n acc -> acc <> singleton (ownCurrencySymbol ctx) (TokenName $ integerToByteString n) 10_000)
--                 mempty
--                 [0,1..(length $ cdRoleMap contractDatum)])

--     validContractDatum :: Bool
--     validContractDatum =
--       cdCreatePolicy contractDatum == ownCurrencySymbol ctx
--         && all (\(_, tot) -> tot == 0) (cdRoleMap contractDatum)

--     -- This validation allows for more tokens to be minted, shouldn't be a problem
--     -- as long as we always valdiate the token name
--     validMintedValue :: Bool
--     validMintedValue =
--       valueOf (txInfoMint info) (ownCurrencySymbol ctx) "" == 1
--         && all (\i ->
--             valueOf
--               (txInfoMint info)
--               (ownCurrencySymbol ctx)
--               (TokenName $ integerToByteString i) == 10_000 
--            ) [0,1..(length $ cdRoleMap contractDatum)]

{-# INLINABLE mkSignPolicy #-}
mkSignPolicy :: (ValidatorHash, Integer) -> ScriptContext -> Bool
mkSignPolicy (vh, role) ctx = True

-- {-# INLINABLE mkSignPolicy #-}
-- mkSignPolicy :: (ValidatorHash, Integer) -> ScriptContext -> Bool
-- mkSignPolicy (vh, role) ctx = validContractValue && validContractDatum
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx

--     input :: TxOut
--     input = case filter predicate ((map txInInfoResolved . txInfoInputs) info) of
--         [o] -> o
--       where
--         predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

--     output :: TxOut 
--     output =  case filter predicate (txInfoOutputs info) of
--         [o] -> o
--       where
--         predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

--     inputDatum :: ContractDatum 
--     inputDatum = case (do
--         dh <- txOutDatumHash input
--         findContractDatum dh (`findDatum` info)) of
--       Just dat -> dat

--     outputDatum :: ContractDatum 
--     outputDatum = case (do
--         dh <- txOutDatumHash output
--         findContractDatum dh (`findDatum` info)) of
--       Just dat -> dat

--     validContractValue :: Bool
--     validContractValue =
--       txOutValue output
--         == txOutValue input
--           <> fst (cdRoleMap inputDatum !! role)
--           <> negate (singleton (cdCreatePolicy inputDatum) (TokenName $ integerToByteString role) 1)

--     validContractDatum :: Bool
--     validContractDatum =
--       outputDatum == ContractDatum
--         { cdCreatePolicy = cdCreatePolicy inputDatum,
--           cdRules = cdRules inputDatum,
--           cdRoleMap = addMember (cdRoleMap inputDatum) role,
--           cdRequests = cdRequests inputDatum,
--           cdExecutions = cdExecutions inputDatum,
--           cdFallbacks = cdFallbacks inputDatum,
--           cdFrozen = cdFrozen inputDatum 
--         }


{-# INLINABLE mkActionPolicy #-}
mkActionPolicy :: (ValidatorHash, ActionRedeemer) -> ScriptContext -> Bool
mkActionPolicy (vh, ar) ctx = True


-- {-# INLINABLE mkActionPolicy #-}
-- mkActionPolicy :: (ValidatorHash, ActionRedeemer) -> ScriptContext -> Bool
-- mkActionPolicy (vh, ar) ctx = validContractValue && validContractDatum && validActor
--   where
--     info :: TxInfo
--     info = scriptContextTxInfo ctx

--     input :: TxOut
--     input = case filter predicate ((map txInInfoResolved . txInfoInputs) info) of
--         [o] -> o
--       where
--         predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

--     output :: TxOut 
--     output =  case filter predicate (txInfoOutputs info) of
--         [o] -> o
--       where
--         predicate (TxOut addr _ _) = toValidatorHash addr == Just vh

--     inputDatum :: ContractDatum 
--     inputDatum = case (do
--         dh <- txOutDatumHash input
--         findContractDatum dh (`findDatum` info)) of
--       Just dat -> dat

--     outputDatum :: ContractDatum 
--     outputDatum = case (do
--         dh <- txOutDatumHash output
--         findContractDatum dh (`findDatum` info)) of
--       Just dat -> dat

--     distribution :: Action
--     distribution = case ar of
--       Execution idx idx' -> snd (cdExecutions inputDatum !! idx) !! idx'
--       Fallback idx idx' -> snd (cdFallbacks inputDatum !! idx) !! idx'
--       Request idx idx' idx'' -> snd (snd (cdRequests inputDatum !! idx) !! idx') !! idx'' 

--     validContractValue :: Bool
--     validContractValue = case distribution of
--       StatusQuo -> txOutValue input == txOutValue output 
--       Distribution  -- Cannot distribute to roles!!!!
--       -- Distribution [(Member, Reward)] | StatusQuo

--     validContractDatum :: Bool
--     validContractDatum = outputDatum == inputDatum 

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


signPolicy :: Scripts.MintingPolicy
signPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
  where 
    wrap = Scripts.wrapMintingPolicy mkSignPolicy

signSymbol :: CurrencySymbol
signSymbol = scriptCurrencySymbol signPolicy 

signScript :: Script
signScript = unMintingPolicyScript signPolicy

signValidator :: Validator
signValidator = Validator signScript


actionPolicy :: Scripts.MintingPolicy
actionPolicy = mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
  where 
    wrap = Scripts.wrapMintingPolicy mkActionPolicy

actionSymbol :: CurrencySymbol
actionSymbol = scriptCurrencySymbol actionPolicy

actionScript :: Script
actionScript = unMintingPolicyScript actionPolicy

actionValidator :: Validator
actionValidator = Validator actionScript

PlutusTx.makeLift ''Member
PlutusTx.makeIsDataIndexed ''Member [('User,0), ('Role,1)]

PlutusTx.makeLift ''Reward
PlutusTx.makeIsDataIndexed ''Reward [('Weight,0), ('Amount,1)]

PlutusTx.makeLift ''Action
PlutusTx.makeIsDataIndexed ''Action [('Distribution,0),('StatusQuo,1)]

PlutusTx.makeLift ''ContractDatum
PlutusTx.makeIsDataIndexed ''ContractDatum [('ContractDatum,0)]

PlutusTx.makeLift ''ActionRedeemer
PlutusTx.makeIsDataIndexed ''ActionRedeemer [('Execution,0), ('Fallback,1), ('Request,2)]

-- Serialization

contractValidatorSerialized :: [CurrencySymbol] -> B.ByteString
contractValidatorSerialized tkts = B16.encode $ serialiseToCBOR ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Plutus.unValidatorScript $ contractValidator tkts) :: PlutusScript PlutusScriptV1)

createSerialized :: TxOutRef -> B.ByteString
createSerialized oref = B16.encode $ serialiseToCBOR ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Plutus.unMintingPolicyScript $ createPolicy oref) :: PlutusScript PlutusScriptV1)

signSerialized :: B.ByteString
signSerialized = B16.encode $ serialiseToCBOR ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Plutus.unMintingPolicyScript signPolicy) :: PlutusScript PlutusScriptV1)

actionSerialized :: B.ByteString
actionSerialized = B16.encode $ serialiseToCBOR ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Plutus.unMintingPolicyScript actionPolicy) :: PlutusScript PlutusScriptV1)

-- Datum

-- sampleContractDatum :: ContractDatum
-- sampleContractDatum = ContractDatum
--   { cdCreatePolicy = createSymbol (TxOutRef "aaa" 0)
--   , cdRules = "bbb"
--   , cdRoleMap = [(mempty, 0)]
--   , cdRequests = [(Role 0, [(Role 1, [StatusQuo])])]
--   , cdExecutions = [(Role 1, [StatusQuo])]
--   , cdFallbacks = [(Role 1, [StatusQuo])]
--   , cdFrozen = True
--   }

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
tickets = [signSymbol, actionSymbol] 

main :: Prelude.IO ()
main = do
  Prelude.putStrLn $ "Contract Validator:\n" ++ Prelude.show (contractValidatorSerialized tickets)
  Prelude.putStrLn $ "Sign Policy:\n" ++ Prelude.show signSerialized
  Prelude.putStrLn $ "Action Policy:\n" ++ Prelude.show actionSerialized
  -- writeJSON "./contract.json" sampleContractDatum


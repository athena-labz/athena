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

import Cardano.Api hiding (Value, TxOut)
import Cardano.Api.Shelley hiding (Value, TxOut)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import Data.Aeson hiding (Value)
import GHC.Generics
import Ledger
import Ledger.Typed.Scripts as Scripts hiding (validatorHash)
import Ledger.Value
import Codec.Serialise hiding (encode)
import qualified Prelude
import qualified PlutusTx
import qualified PlutusTx.AssocMap as PlutusMap
import qualified Plutus.V1.Ledger.Api as Plutus
import PlutusTx (Data (..))
import PlutusTx.Prelude

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
  { cdNFT :: TokenName,
    cdRules :: BuiltinByteString,
    cdRoleMap :: [(Integer, (Collateral, TotalMembers))],
    cdRequests :: RequesterMap,
    cdExecutions :: DistributionMap,
    cdFallbacks :: DistributionMap,
    cdFrozen :: Bool
  }
  deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

instance Eq ContractDatum where
  {-# INLINEABLE (==) #-}
  ContractDatum nft rls rm rq ex fb fz 
    == ContractDatum nft' rls' rm' rq' ex' fb' fz' =
      nft == nft' && rls == rls' && rm == rm' && rq == rq'
        && ex == ex' && fb == fb' && fz == fz'

{-# INLINEABLE mkContractValidator #-}
mkContractValidator ::
  ContractDatum ->
  CurrencySymbol ->
  ScriptContext ->
  Bool
mkContractValidator dat tkt ctx = True

data ContractType

instance Scripts.ValidatorTypes ContractType where
  type DatumType ContractType = ContractDatum
  type RedeemerType ContractType = CurrencySymbol

typedContractValidator :: Scripts.TypedValidator ContractType
typedContractValidator =
  Scripts.mkTypedValidator @ContractType
    $$(PlutusTx.compile [||mkContractValidator||])
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @ContractDatum @CurrencySymbol

contractValidator :: Validator
contractValidator = Scripts.validatorScript typedContractValidator

contractValidatorHash :: ValidatorHash
contractValidatorHash = validatorHash contractValidator

contractAddress :: Ledger.Address
contractAddress = scriptAddress contractValidator

PlutusTx.makeLift ''Member
PlutusTx.makeIsDataIndexed ''Member [('User,0), ('Role,1)]

PlutusTx.makeLift ''Reward
PlutusTx.makeIsDataIndexed ''Reward [('Weight,0), ('Amount,1)]

PlutusTx.makeLift ''Action
PlutusTx.makeIsDataIndexed ''Action [('Distribution,0),('StatusQuo,1)]

PlutusTx.makeLift ''ContractDatum
PlutusTx.makeIsDataIndexed ''ContractDatum [('ContractDatum,0)]

-- Serialization

contractValidatorSerialized :: B.ByteString
contractValidatorSerialized = B16.encode $ serialiseToCBOR ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Plutus.unValidatorScript contractValidator) :: PlutusScript PlutusScriptV1)

-- Datum

sampleContractDatum :: ContractDatum
sampleContractDatum = ContractDatum
  { cdNFT = "aaa"
  , cdRules = "bbb"
  , cdRoleMap = [(0, (mempty, 0))]
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

main :: Prelude.IO ()
main = do
  Prelude.putStrLn $ "ContractValidator:\n" ++ Prelude.show contractValidatorSerialized
  writeJSON "./contract.json" sampleContractDatum


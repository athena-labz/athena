{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Cardano.Api                    hiding (Value, TxOut, Script)
import           Cardano.Api.Shelley            hiding (Value, TxOut, Script)
import           Codec.Serialise                hiding (encode)
import GHC.Generics
import           Data.Aeson                     as A
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import           Data.Functor                   (void)
import qualified Ledger.Typed.Scripts           as Scripts
import qualified Ledger                         as L
import qualified Plutus.Script.Utils.V2.Scripts as PSU.V2
import qualified Plutus.Script.Utils.V1.Scripts as PSU.V1
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import qualified Plutus.V1.Ledger.Api           as PlutusV1
import           Plutus.V2.Ledger.Contexts      (ownHash, findOwnInput)
import           Ledger.Value (flattenValue)
import 		 PlutusTx.AssocMap              as PlutusMap
import qualified PlutusTx
import           PlutusTx.Prelude               as P hiding (Semigroup (..),
                                                      unless, (.))
import           Prelude                        (IO, Semigroup (..), print, (.), FilePath, Show, Eq)


{-
   The validator script
-}

data ContractDatum = ContractDatum
  { cdMediators :: !PlutusV1.CurrencySymbol
  , cdTarget :: !PlutusV1.CurrencySymbol
  , cdFallback :: !PlutusV1.CurrencySymbol
  , cdDeadline :: !PlutusV1.POSIXTime -- Convert in the sc to posix time
  }
  deriving (Prelude.Show, Generic, A.FromJSON, A.ToJSON, Prelude.Eq)

instance P.Eq ContractDatum where
  {-# INLINABLE (==) #-}
  ContractDatum m t f d == ContractDatum m' t' f' d' =
    m == m' && t == t' && f == f' && d == d'

data ContractRedeemer = ExecuteTarget | ExecuteFallback
  deriving (Prelude.Show, Generic, A.FromJSON, A.ToJSON, Prelude.Eq)

instance P.Eq ContractRedeemer where
  {-# INLINABLE (==) #-}
  ExecuteTarget == ExecuteTarget = True
  ExecuteFallback == ExecuteFallback = True
  _ == _ = False

{-# INLINABLE tokenCountOf #-}
tokenCountOf :: PlutusV2.CurrencySymbol -> PlutusV2.Value -> Integer
tokenCountOf targetSymbol val =
  case PlutusMap.lookup targetSymbol (PlutusV2.getValue val) of
    Nothing     -> 0
    Just tokens -> foldr (\acc amt -> amt + acc) 0 $ PlutusMap.elems tokens

{-# INLINEABLE alwaysValidateLogic #-}
alwaysValidateLogic :: ContractDatum -> ContractRedeemer -> PlutusV2.ScriptContext -> Bool
alwaysValidateLogic dat red ctx =
    traceIfFalse "wrong output" rightOutput
    && traceIfFalse "deadline not reached" deadlineReached
      where
    info :: PlutusV2.TxInfo
    info = PlutusV2.scriptContextTxInfo ctx

    input :: PlutusV2.TxOut
    input = case findOwnInput ctx of
      Just inp -> PlutusV2.txInInfoResolved inp 

    output :: PlutusV2.TxOut
    output = case P.filter predicate (PlutusV2.txInfoOutputs info) of
        [o] -> o
        _ -> traceError "output from expected address not found"
      where
        predicate (PlutusV2.TxOut addr _ _ _) = addr == PlutusV2.txOutAddress referenceInput

    referenceInput :: PlutusV2.TxOut
    referenceInput = case P.find predicate (PlutusV2.txInfoReferenceInputs info) of
        Just ref -> PlutusV2.txInInfoResolved ref
        _ -> traceError "reference input not found"
      where
        predicate (PlutusV2.TxInInfo _ (PlutusV2.TxOut _ val _ _)) = case red of
	  ExecuteTarget -> tokenCountOf (cdTarget dat) val == 1
	  ExecuteFallback -> tokenCountOf (cdFallback dat) val == 1

    rightOutput :: Bool
    rightOutput = PlutusV2.txOutValue output == PlutusV2.txOutValue input

    deadlineReached :: Bool
    deadlineReached = not $ (cdDeadline dat) `L.member` PlutusV1.Interval (PlutusV1.ivFrom $ PlutusV2.txInfoValidRange info) (PlutusV1.UpperBound PlutusV1.PosInf True)

{-# INLINEABLE alwaysFailLogic #-}
alwaysFailLogic :: () -> () -> PlutusV2.ScriptContext -> Bool
alwaysFailLogic _ _ _ = False

{-
    As a validator
-}

alwaysValidatePolicy :: Scripts.Validator
alwaysValidatePolicy = PlutusV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
     where
         wrap = PSU.V2.mkUntypedValidator alwaysValidateLogic

alwaysFailPolicy :: Scripts.Validator
alwaysFailPolicy = PlutusV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
     where
         wrap = PSU.V2.mkUntypedValidator alwaysFailLogic

{-
   As a Short Byte String
-}

alwaysValidateScriptSBS :: SBS.ShortByteString
alwaysValidateScriptSBS = SBS.toShort . LBS.toStrict $ serialise $ alwaysValidatePolicy

alwaysFailScriptSBS :: SBS.ShortByteString
alwaysFailScriptSBS = SBS.toShort . LBS.toStrict $ serialise $ alwaysFailPolicy

{-
   As a Serialised Script
-}

alwaysValidateSerialisedScript :: PlutusScript PlutusScriptV2
alwaysValidateSerialisedScript = PlutusScriptSerialised alwaysValidateScriptSBS

alwaysFailSerialisedScript :: PlutusScript PlutusScriptV2
alwaysFailSerialisedScript = PlutusScriptSerialised alwaysFailScriptSBS

writeAlwaysValidateSerialisedScript :: IO ()
writeAlwaysValidateSerialisedScript = void $ writeFileTextEnvelope "always-validate.plutus" Nothing alwaysValidateSerialisedScript

writeAlwaysFailSerialisedScript :: IO ()
writeAlwaysFailSerialisedScript = void $ writeFileTextEnvelope "always-fail.plutus" Nothing alwaysFailSerialisedScript

{-
    Datum
-}

sampleContractDatum =
  ContractDatum
    (PlutusV1.adaSymbol)
    (PlutusV1.adaSymbol)
    (PlutusV1.adaSymbol)
    1660002044

PlutusTx.makeLift ''ContractDatum
PlutusTx.makeIsDataIndexed ''ContractDatum [('ContractDatum,0)]

PlutusTx.makeLift ''ContractRedeemer
PlutusTx.makeIsDataIndexed ''ContractRedeemer [('ExecuteTarget,0), ('ExecuteFallback,1)]

dataToScriptData :: PlutusTx.Data -> ScriptData
dataToScriptData (PlutusTx.Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (PlutusTx.Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (PlutusTx.List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (PlutusTx.I n)         = ScriptDataNumber n
dataToScriptData (PlutusTx.B bs)        = ScriptDataBytes bs

writeJSON :: PlutusTx.ToData a => Prelude.FilePath -> a -> Prelude.IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

main :: IO ()
main = do
   writeAlwaysValidateSerialisedScript
   writeAlwaysFailSerialisedScript
   writeJSON "datum.json" sampleContractDatum

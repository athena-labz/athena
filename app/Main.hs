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

import           Cardano.Api                    (PlutusScriptV2,
                                                 writeFileTextEnvelope)
import           Cardano.Api.Shelley            (PlutusScript (..),
                                                 ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                                 fromPlutusData,
                                                 scriptDataToJson)
import           Codec.Serialise
import           Data.Aeson                     as A
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import           Data.Functor                   (void)
import qualified Ledger.Typed.Scripts           as Scripts
import qualified Plutus.Script.Utils.V2.Scripts as PSU.V2
import qualified Plutus.V2.Ledger.Api           as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude               as P hiding (Semigroup (..),
                                                      unless, (.))
import           Prelude                        (IO, Semigroup (..), print, (.))


{-
   The validator script
-}

{-# INLINEABLE alwaysValidatePolicy #-}
alwaysValidatePolicy :: () -> () -> PlutusV2.ScriptContext -> Bool
alwaysValidatePolicy _ _ _ = True

{-
    As a validator
-}

policy :: Scripts.Validator
policy = PlutusV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
     where
         wrap = PSU.V2.mkUntypedValidator alwaysValidatePolicy

{-
   As a Short Byte String
-}

scriptSBS :: SBS.ShortByteString
scriptSBS = SBS.toShort . LBS.toStrict $ serialise $ policy

{-
   As a Serialised Script
-}

serialisedScript :: PlutusScript PlutusScriptV2
serialisedScript = PlutusScriptSerialised scriptSBS

writeSerialisedScript :: IO ()
writeSerialisedScript = void $ writeFileTextEnvelope "always-validate.plutus" Nothing serialisedScript

main :: IO ()
main = writeSerialisedScript
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module NFT.OffChain where

import           Control.Monad
import qualified Data.Map           as HaskellMap
import           Data.Text          (Text)
import           Data.Void          (Void)
import           Ledger             (Address, TokenName, TxId (getTxId),
                                     TxOutRef (..), Value, Redeemer (..),
                                     pubKeyHashAddress)
import           Ledger.Constraints as Constraints (ScriptLookups,
                                                    TxConstraints,
                                                    mintingPolicy,
                                                    mustMintValueWithRedeemer,
                                                    mustSpendPubKeyOutput,
                                                    unspentOutputs)
import           Ledger.Value       as Value (TokenName (TokenName), singleton)
import           Plutus.Contract    as Contract (Contract, Endpoint,
                                                 Promise (awaitPromise),
                                                 endpoint, handleError,
                                                 logError, logInfo,
                                                 ownPaymentPubKeyHash,
                                                 submitTxConstraintsWith,
                                                 utxosAt)
import           PlutusTx.Prelude   (BuiltinByteString, Maybe (..), Integer,
                                     appendByteString, lengthOfByteString,
                                     sliceByteString, otherwise,
                                     ($), (==), (/=), (<>))
import           NFT.NFT
import qualified Prelude            as Haskell
import qualified PlutusTx
import           Text.Printf        (printf)
import           NFT.NFTUtils

mintNFT :: Integer -> Contract () NFTSchema Text ()
mintNFT size = do
  -- The public key hash from the user who is trying to create an account
  pmtPkh <- Contract.ownPaymentPubKeyHash

  let --
      addr :: Address
      addr = pubKeyHashAddress pmtPkh Nothing

  utxos <- utxosAt addr
  case HaskellMap.keys utxos of
    [] -> Contract.logError @Haskell.String "no utxo found"
    oref : _ -> do
      let --
          tn :: TokenName
          tn = TokenName $ parsedOutput
            where
              parsedOutputId :: BuiltinByteString
              parsedOutputId = case intToBuiltinByteString $ txOutRefIdx oref of
                bbs | lengthOfByteString bbs == 1 -> "0" `appendByteString` bbs
                    | otherwise -> bbs

              parsedOutput :: BuiltinByteString
              parsedOutput =
                sliceByteString 0 size (getTxId $ txOutRefId oref)
                  `appendByteString` parsedOutputId

          val :: Value
          val = Value.singleton (nftCurrencySymbol size) tn 1

          lookups :: ScriptLookups Void
          lookups =
            Constraints.mintingPolicy (nftPolicy size)
              Haskell.<> Constraints.unspentOutputs utxos

          tx :: TxConstraints Void Void
          tx =
            Constraints.mustMintValueWithRedeemer (Redeemer $ PlutusTx.toBuiltinData oref) val
              <> Constraints.mustSpendPubKeyOutput oref

      void $ submitTxConstraintsWith @Void lookups tx
      Contract.logInfo @Haskell.String $ printf "forged %s" (Haskell.show val)

type NFTSchema = Endpoint "mint-nft" Integer

nftEndpoints :: Contract () NFTSchema Text ()
nftEndpoints =
  forever $ handleError logError $ awaitPromise $ createAccount'
 where
  createAccount' = endpoint @"mint-nft" $ \size -> mintNFT size

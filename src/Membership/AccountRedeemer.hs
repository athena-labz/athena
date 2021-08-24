{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Membership.AccountRedeemer where

import Ledger ( TxOutRef )
import qualified Prelude
import qualified PlutusTx
import Membership.ContractDatum (ContractDatum (..))

data AccountRedeemer = Create | Sign | Collect
    deriving Prelude.Show

PlutusTx.unstableMakeIsData ''AccountRedeemer
{-# LANGUAGE NumericUnderscores #-}

module Membership.Utils where

import PlutusTx.Prelude ( Bool(..), Maybe(..) )
import qualified Prelude ()

{-# INLINEABLE tripleSnd #-}
tripleSnd :: (a, b, c) -> b
tripleSnd (_, x, _) = x

{-# INLINABLE assertTrue #-}
assertTrue :: Bool -> Maybe ()
assertTrue True = Just ()
assertTrue False = Nothing
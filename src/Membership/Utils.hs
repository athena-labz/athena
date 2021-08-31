{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Membership.Utils where

import PlutusTx.Prelude ( Eq, Maybe(..), not, ($), find, return)
import PlutusTx.AssocMap as Map ( lookup, Map, keys, member )
import qualified Prelude ()

{-# INLINEABLE tripleFst #-}
tripleFst :: (a, b, c) -> a
tripleFst (a, _, _) = a

{-# INLINEABLE tripleSnd #-}
tripleSnd :: (a, b, c) -> b
tripleSnd (_, b, _) = b

{-# INLINEABLE tripleThd #-}
tripleThd :: (a, b, c) -> c
tripleThd (_, _, c) = c

{-# INLINABLE subtractMaps #-}
subtractMaps :: forall k v. (Eq k) => Map.Map k v -> Map.Map k v -> Maybe (k, v)
subtractMaps m m' = do
    k <- find (\k' -> not $ k' `Map.member` m') (keys m)
    v <- lookup k m
    return (k, v)
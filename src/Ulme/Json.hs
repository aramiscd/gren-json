{- |
    Copyright   : (c) 2019-2022 Aramís Concepción Durán
    License     : GPL-3.0-only
    Maintainer  : Aramís Concepción Durán <aramis@systemli.org>
-}
module Ulme.Json
    ( Json ( JsonAtom , JsonArray , JsonObject , JsonEmpty )
    )
where

import Ulme

import Data.Monoid qualified as Monoid
import Data.Semigroup ( (<>) )


{-
    Data type for representing JSON documents.
-}
data Json
    = JsonAtom String
    | JsonArray ( List Json )
    | JsonObject ( List ( Json , Json ) )
    | JsonEmpty
    deriving ( Show )


instance Semigroup Json where
    JsonEmpty <> right = right
    left <> JsonEmpty = left
    left <> right = JsonArray [ left , right ]


instance Monoid Json where
    mempty = JsonEmpty

{-
    This module provides a `Json` data type

    ----

    Copyright 2019-2022, Aramis Concepcion Duran

    This file is part of ulme-json.

    Ulme-json is free software: you can redistribute it
    and/or modify it under the terms of the GNU General
    Public License as published by the Free Software
    Foundation, either version 3 of the License, or (at
    your option) any later version.

    Ulme-json is distributed in the hope that it will be
    useful, but WITHOUT ANY WARRANTY; without even the
    implied warranty of MERCHANTABILITY or FITNESS FOR
    A PARTICULAR PURPOSE.  See the GNU General Public
    License for more details.

    You should have received a copy of the GNU General
    Public License along with Foobar.  If not, see
    <https://www.gnu.org/licenses/>.
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

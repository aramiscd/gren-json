module Ulme.Json

{-
    This module provides a `Json` data type


    ----

    Copyright 2019-2020, Aramis Concepcion Duran
    
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

( Json ( Jatom , Jarray , Jobject )
)
where


import Ulme


data Json
{-
    Data type for representing JSON Documents.
-}
    = Jatom String
    | Jarray ( List Json )
    | Jobject ( List ( Json , Json ) )
    deriving Show
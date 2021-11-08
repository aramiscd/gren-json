{-
    Pretty-print a JSON document.

    I like how `elm-format` lays out commas as prefixes:

        [ foo
        , bar
        , baz
        ]

    No existing JSON pretty-printer supported this layout,
    so I wrote my own.  It produces the same output
    for all semantically equivalent JSON documents.

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

module Ulme.Json.Pretty (
    print,
    printValue,
    printArray,
    printObject,
    printMember,
) where

import Ulme

import Ulme.Json (Json (JsonArray, JsonAtom, JsonEmpty, JsonObject))
import Ulme.String qualified as String


print :: Json -> String
{-
    Pretty-print a JSON value.
-}
print value =
    printValue value |> String.join "\n"


printValue :: Json -> List String
{-
    Pretty-print a JSON value as a list of strings.
-}
printValue value =
    case value of
        JsonEmpty -> []
        JsonAtom atom -> [atom]
        JsonArray elements -> printArray elements
        JsonObject members -> printObject members


printArray :: List Json -> List String
{-
    Pretty-print a JSON array as a list of strings.
-}
printArray array =
    case array of
        [] -> ["[]"]
        firstElement : elements ->
            printElement True firstElement
                ++ (elements >>= printElement False)
                ++ ["]"]


printElement :: Bool -> Json -> List String
{-
    Pretty-print a JSON array element as a list of strings.
-}
printElement isFirst element =
    let prefix = if isFirst then "[ " else ", "
     in printValue element |> \ case
            [] -> []
            firstLine : lines -> [prefix ++ firstLine] ++ map ("  " ++) lines


printObject :: List (Json, Json) -> List String
{-
    Pretty-print a JSON object as a list of strings.
-}
printObject object =
    case object of
        [] -> ["{}"]
        firstMember : members ->
            printMember True firstMember
                ++ (members >>= printMember False)
                ++ ["}"]


printMember :: Bool -> (Json, Json) -> List String
{-
    Pretty-print a JSON object member as a list of strings.
-}
printMember isFirst (key, value) =
    let prefix = if isFirst then "{ " else ", "
     in printValue key |> \ case
            [] -> []
            k : _ ->
                case printValue value of
                    [line] -> [prefix ++ k ++ " : " ++ line]
                    lines -> [prefix ++ k ++ " :"] ++ map ("    " ++) lines

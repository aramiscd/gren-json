{- |
    Copyright   : (c) 2019-2022 Aramís Concepción Durán
    License     : GPL-3.0-only
    Maintainer  : Aramís Concepción Durán <aramis@systemli.org>
-}
module Ulme.Json.Parse
    ( skip
    , list
    , whitespace
    , oneNine
    , digit
    , digits
    , integer
    , fraction
    , sign
    , exponent
    , number
    , hex
    , escape
    , char
    , character
    , characters
    , string
    , bool
    , null
    , atom
    , value
    , element
    , elements
    , array
    , stringAtom
    , member
    , members
    , object
    , json
    )
where

import Ulme

import Data.Monoid ( mempty )
import Ulme.Char qualified as Char
import Ulme.Json ( Json ( JsonArray , JsonAtom , JsonObject ) )
import Ulme.List qualified as List
import Ulme.Parse ( Parsed ( Parsed ) , Parser , parse )
import Ulme.Parse qualified as Parse
import Ulme.String qualified as String


skip :: Monoid b => Parser a -> Parser b
skip parser =
    Parse.map ( always mempty ) parser


list :: Parser a -> Parser ( List a )
list parser =
    Parse.map List.singleton parser


whitespace :: Monoid a => Parser a
{- ^
    Parse whitespace (greedy).
-}
whitespace =
    skip
        ( Parse.zeroOrMore
            ( Parse.oneOf
                [ Parse.string "\x0020" -- space
                , Parse.string "\x000D" -- carriage return
                , Parse.string "\x000A" -- line feed
                , Parse.string "\x0009" -- tab
                ]
            )
        )


-- Integers

oneNine :: Parser String
{- ^
    Parse a non-zero decimal digit.
-}
oneNine =
    Parse.oneOf
        [ Parse.string "1"
        , Parse.string "2"
        , Parse.string "3"
        , Parse.string "4"
        , Parse.string "5"
        , Parse.string "6"
        , Parse.string "7"
        , Parse.string "8"
        , Parse.string "9"
        ]


digit :: Parser String
{- ^
    Parse a decimal digit.
-}
digit =
    Parse.oneOf
        [ Parse.string "0"
        , oneNine
        ]


digits :: Parser String
{- ^
    Parse as many decimal digits as possible but at
    least one.
-}
digits =
    Parse.oneOrMore digit


integer :: Parser String
{- ^
    Parse an integer.

    Only decimal notation, no leading zeros, with or without a leading minus.
-}
integer =
    Parse.oneOf
        [ Parse.sequence [ oneNine , digits ]
        , digit
        , Parse.sequence [ Parse.string "-" , oneNine , digits ]
        , Parse.sequence [ Parse.string "-" , digit ]
        ]


-- Floating-point numbers

fraction :: Parser String
{- ^
    Parse the fractional part of a floating-point number.
-}
fraction =
    Parse.sequence
        [ Parse.string "."
        , digits
        ]


sign :: Parser String
{- ^
    Parse the sign of a scientific exponential suffix.

    This is NOT the optional sign in front of a JSON number.
-}
sign =
    Parse.optional
        ( Parse.oneOf
            [ Parse.string "+"
            , Parse.string "-"
            ]
        )


exponent :: Parser String
{- ^
    Parse the scientific exponential suffix of a number (if any).
-}
exponent =
    Parse.sequence
        [ Parse.oneOf [ Parse.string "E" , Parse.string "e" ]
        , sign
        , digits
        ]


number :: Parser String
{- ^
    Parse a JSON number.

    JSON numbers are either integer or floating-point, with or without a scientific exponential suffix.  Only decimal
    notation is valid: no hex, no octal.
-}
number =
    Parse.sequence
        [ integer
        , Parse.optional fraction
        , Parse.optional exponent
        ]


-- Strings

hex :: Parser String
{- ^
    Parse a hexadecimal digit.
-}
hex =
    Parse.oneOf
        [ digit
        , Parse.oneOf
            [ Parse.string "A"
            , Parse.string "B"
            , Parse.string "C"
            , Parse.string "D"
            , Parse.string "E"
            , Parse.string "F"
            , Parse.string "a"
            , Parse.string "b"
            , Parse.string "c"
            , Parse.string "d"
            , Parse.string "e"
            , Parse.string "f"
            ]
        ]


escape :: Parser String
{- ^
    Parse an escape character.
-}
escape =
    Parse.oneOf
        [ Parse.string "\""
        , Parse.string "\\"
        , Parse.string "/"
        , Parse.string "b"
        , Parse.string "f"
        , Parse.string "n"
        , Parse.string "r"
        , Parse.string "t"
        , Parse.sequence [ Parse.string "u" , hex , hex , hex , hex ]
        ]


char :: Parser String
{- ^
    Parse an unescaped character.
-}
char input =
    case String.uncons input of
        Nothing -> Parse.fail input
        Just ( head , _ ) -> do
            let c = Char.toCode head
            if c < 32 || c == 34 || c == 92 || c > 1114111
            then Parse.fail input
            else Parse.string ( String.fromChar head ) input


character :: Parser String
{- ^
    Parse a character.
-}
character =
    Parse.oneOf
        [ char
        , Parse.sequence [ Parse.string "\\" , escape ]
        ]


characters :: Parser String
{- ^
    Parse any number of characters.
-}
characters =
    Parse.zeroOrMore character


string :: Parser String
{- ^
    Parse a JSON string.
-}
string =
    Parse.sequence
        [ Parse.string "\""
        , characters
        , Parse.string "\""
        ]


-- Booleans and null

bool :: Parser String
{- ^
    Parse `true` or `false`.
-}
bool =
    Parse.oneOf
        [ Parse.string "true"
        , Parse.string "false"
        ]


null :: Parser String
{- ^
    Parse `null`.
-}
null =
    Parse.string "null"


-- JSON values

atom :: Parser Json
{- ^
    Parse an atomic JSON value into a `Json` value.
-}
atom =
    -- todo: lieber separate konstruktoren?
    Parse.oneOf [ string , number , bool , null ]
        |> Parse.map JsonAtom


value :: Parser Json
{- ^
    Parse a JSON value into a `Json` value.
-}
value =
    Parse.oneOf [ object , array , atom ]


-- JSON arrays

element :: Parser Json
{- ^
    Parse an element of a JSON array.
-}
element =
    Parse.sequence [ whitespace , value , whitespace ]


elements :: Parser ( List Json )
{- ^
    Parse elements of a JSON array.
-}
elements =
    Parse.sequence
        [ list element
        , Parse.optional
            ( Parse.sequence
                [ skip ( Parse.string "," )
                , elements
                ]
            )
        ]


array :: Parser Json
{- ^
    Parse a JSON array into a `JsonArray`.
-}
array =
    Parse.sequence
        [ skip ( Parse.string "[" )
        , Parse.oneOf [ elements , whitespace ]
        , skip ( Parse.string "]" )
        ]
        |> Parse.map JsonArray


-- JSON objects

stringAtom :: Parser Json
{- ^
    Parse a JSON string into a `Json` value.

    This is a version of `atom` that only parses strings.  We need this because the keys of JSON objects are only
    allowed to be strings.
-}
stringAtom =
    Parse.sequence
        [ whitespace
        , Parse.map JsonAtom string
        , whitespace
        ]


member :: Parser ( Json , Json )
{- ^
    Parse a member of a JSON object.
-}
member input =
    case Parse.sequence [ stringAtom , skip ( Parse.string ":" ) , element ] input of
        Parsed ( Parse.Partial { Parse.value = JsonArray [ key , val ] , Parse.backlog = backlog } ) ->
            Parsed ( Parse.Partial { Parse.value = ( key , val ) , Parse.backlog = backlog } )
        _anythingElse ->
            Parse.Fail


members :: Parser ( List ( Json , Json ) )
{- ^
    Parse the members of a JSON object.
-}
members =
    Parse.sequence
        [ list member
        , Parse.optional
            ( Parse.sequence
                [ skip ( Parse.string "," )
                , members
                ]
            )
        ]


object :: Parser Json
{- ^
    Parse a JSON object.
-}
object =
    Parse.sequence
        [ skip ( Parse.string "{" )
        , Parse.map JsonObject ( Parse.oneOf [ members , whitespace ] )
        , skip ( Parse.string "}" )
        ]


-- JSON document

json :: String -> Maybe Json
{- ^
    Parse an entire JSON document.
-}
json =
    parse element

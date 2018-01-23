module Test.Decimal exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, floatRange, custom)
import Random.Pcg as Random exposing (Generator)
import Test exposing (..)
import Shrink exposing (Shrinker)
import Lazy.List exposing (empty, (:::))
import Decimal exposing (..)
import Integer
import Test.Integer exposing (intString, maxIntRange)


-- Generates string with 10 ^ 20 integer and 10 ^ 20 fraction parts


floatString : Fuzzer String
floatString =
    custom floatStringGenerator floatStringShrinker


floatStringGenerator : Generator String
floatStringGenerator =
    let
        int =
            Random.list 2 (Random.int 0 Random.maxInt)
                |> Random.map (List.map Basics.toString)
                |> Random.map (List.foldr (++) "")

        fraction =
            Random.list 2 (Random.int 0 Random.maxInt)
                |> Random.map (List.map Basics.toString)
                |> Random.map (List.foldr (++) "")

        sign =
            Random.choice "" "-"
    in
        Random.map2 (++) (Random.constant ".") fraction
            |> Random.map2 (++) int
            |> Random.map2 (++) sign


floatStringShrinker : Shrinker String
floatStringShrinker s =
    let
        baseValues : List String
        baseValues =
            let
                ints =
                    List.range -9 9
                        |> List.map toFloat

                floats =
                    List.range -9 9
                        |> List.map toFloat
                        |> List.map (flip (/) 10)
            in
                List.append ints floats
                    |> List.map Basics.toString
                    |> (::) "0.0"

        isNegative : String -> Bool
        isNegative s =
            String.startsWith "-" s

        shrinkInt : String -> String
        shrinkInt s =
            if String.length s == 1 then
                "0"
            else
                String.dropRight 1 s

        shrinkFraction : String -> String
        shrinkFraction s =
            String.dropRight 1 s

        sign =
            if isNegative s then
                "-"
            else
                ""

        num =
            if isNegative s then
                String.dropLeft 1 s
            else
                s

        sepIndex =
            String.indexes "." s |> List.head

        ( int, fraction ) =
            case sepIndex of
                Nothing ->
                    ( s, "" )

                Just i ->
                    ( String.left (i - 1) s, String.dropLeft i s )

        s_ =
            if String.isEmpty fraction then
                sign ++ (shrinkInt int)
            else
                (sign ++ (shrinkInt int)) ++ ("." ++ (shrinkFraction fraction))
    in
        if List.member s baseValues then
            empty
        else
            s_ ::: empty


suite : Test
suite =
    describe "Decimal module"
        [ describe "fromInt"
            [ fuzz maxIntRange "should create correct Decimal" <|
                \i ->
                    Expect.equal (Decimal.toString << fromInt <| i) (Basics.toString i)
            ]
        , describe "fromInteger"
            [ fuzz intString "should create correct Decimal" <|
                \i ->
                    let
                        maybeInteger =
                            Integer.fromString i

                        maybeDecimal =
                            Maybe.map fromInteger maybeInteger
                    in
                        Expect.equal (Maybe.map Decimal.toString maybeDecimal)
                            (Maybe.map Integer.toString
                                maybeInteger
                            )
            ]
        , describe "fromFloat"
            [ fuzz (floatRange (toFloat Random.minInt) (toFloat Random.maxInt)) "should create correct Decimal" <|
                \f ->
                    Expect.equal (Decimal.toString << fromFloat <| f) (Basics.toString f)
            ]
        , describe "fromString"
            [ fuzz floatString "should create correct Decimal" <|
                \f ->
                    Expect.equal (Maybe.map Decimal.toString << fromString <| f) (Just f)
            ]
        ]

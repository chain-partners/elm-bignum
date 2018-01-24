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
        , describe "add"
            [ fuzz2 floatString floatString "should have transitivity property" <|
                \f1 f2 ->
                    let
                        a =
                            fromString f1

                        b =
                            fromString f2
                    in
                        Expect.equal (Maybe.map2 add a b) (Maybe.map2 add b a)
            , fuzz3 floatString floatString floatString "should have associativity property" <|
                \f1 f2 f3 ->
                    let
                        a =
                            fromString f1

                        b =
                            fromString f2

                        c =
                            fromString f3
                    in
                        Expect.equal (Maybe.map2 add (Maybe.map2 add a b) c)
                            (Maybe.map2 add
                                a
                                (Maybe.map2 add b c)
                            )
            , fuzz floatString "should have identity property" <|
                \f ->
                    let
                        a =
                            fromString f

                        b =
                            fromFloat 0
                    in
                        Expect.equal (Maybe.map (add b) a) a
            , fuzz3 floatString floatString floatString "should have distributive property" <|
                \f1 f2 f3 ->
                    let
                        a =
                            fromString f1

                        b =
                            fromString f2

                        c =
                            fromString f3
                    in
                        Expect.equal (Maybe.map2 mul a (Maybe.map2 add b c))
                            (Maybe.map2 add
                                (Maybe.map2 mul a b)
                                (Maybe.map2 mul a c)
                            )
            ]
        , describe "sub"
            [ fuzz floatString "should have identity property" <|
                \f ->
                    let
                        a =
                            fromString f

                        b =
                            Just (fromFloat 0)
                    in
                        Expect.equal (Maybe.map2 sub a b) a
            , fuzz floatString "should have inverse" <|
                \f ->
                    let
                        a =
                            fromString f

                        b =
                            Just (fromInt 0)
                    in
                        Expect.equal (Maybe.map2 sub a a) b
            ]

        {-
           , describe "mul"
               [ fuzz2 floatString floatString "should have transitivity property" <|
                   \f1 f2 ->
                       let
                           a =
                               fromString f1

                           b =
                               fromString f2
                       in
                           Expect.equal (Maybe.map2 mul a b) (Maybe.map2 mul b a)
               , fuzz3 floatString floatString floatString "should have associativity property" <|
                   \f1 f2 f3 ->
                       let
                           a =
                               fromString f1

                           b =
                               fromString f3

                           c =
                               fromString f3
                       in
                           Expect.equal (Maybe.map2 mul (Maybe.map2 mul a b) c)
                               (Maybe.map2 mul
                                   a
                                   (Maybe.map2 mul b c)
                               )
               , fuzz floatString "should have identity property" <|
                   \f ->
                       let
                           a =
                               fromString f

                           b =
                               fromFloat 1
                       in
                           Expect.equal (Maybe.map (mul b) a) a
               , fuzz3 floatString floatString floatString "should have distributive property" <|
                   \f1 f2 f3 ->
                       let
                           a =
                               fromString f1

                           b =
                               fromString f2

                           c =
                               fromString f3
                       in
                           Expect.equal (Maybe.map2 mul a (Maybe.map2 add b c))
                               (Maybe.map2 add
                                   (Maybe.map2 mul a b)
                                   (Maybe.map2 mul a c)
                               )
               ]

        -}
        {-
           , describe "abs"
               [ fuzz floatString "abs i should be larger than or equal to i" <|
                   \f ->
                       case (Maybe.map2 gte (Maybe.map Integer.abs (fromString f)) (fromString f)) of
                           Just True ->
                               Expect.pass

                           _ ->
                               Expect.fail "property does not hold"
               ]
        -}
        , describe "negate"
            [ fuzz floatString "should return original i when applied twice" <|
                \f ->
                    let
                        float =
                            fromString f

                        float_ =
                            float
                                |> Maybe.map Decimal.negate
                                |> Maybe.map Decimal.negate
                    in
                        Expect.equal float float_
            ]

        {-
           , describe "compare"
               [ fuzz2 floatString floatString "should return correct order" <|
                   \f1 f2 ->
                       let
                           a =
                               fromString f1

                           b =
                               fromString f2

                           comparison =
                               Maybe.map2 Integer.compare a b
                       in
                           case comparison of
                               Nothing ->
                                   Expect.fail "was given invalid string for generating Integer"

                               _ ->
                                   Expect.equal (Maybe.map2 Integer.compare (Maybe.map2 sub a b) (Just (fromInt 0))) comparison
               ]
        -}
        ]

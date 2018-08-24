module Test.Decimal exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, floatRange, custom)
import Random.Pcg as Random exposing (Generator)
import Shrink exposing (Shrinker)
import Test exposing (..)
import Lazy.List exposing (empty, (:::))
import Char
import Decimal exposing (..)
import Integer
import Regex
import Test.Integer exposing (intString, maxIntRange)


decimalString : Fuzzer String
decimalString =
    custom decimalStringGenerator decimalStringShrinker


decimalStringGenerator : Generator String
decimalStringGenerator =
    let
        int =
            Random.choices (List.range 1 2 |> List.map numBuilder)

        fraction =
            Random.choices (List.range 1 2 |> List.map numBuilder)

        numBuilder : Int -> Generator String
        numBuilder i =
            Random.list i (Random.int 0 Random.maxInt)
                |> Random.map
                    (List.map Basics.toString
                        >> List.foldr (++) ""
                    )

        sign =
            Random.choice "" "-"
    in
        Random.map2 (++) sign int
            |> Random.map2 (flip (++)) (Random.constant ".")
            |> Random.map2 (flip (++)) fraction
            |> Random.map trimTrailingZero


decimalStringShrinker : Shrinker String
decimalStringShrinker s =
    let
        isMinimalCase : String -> Bool
        isMinimalCase s =
            s
                |> String.filter Char.isDigit
                |> String.length
                |> flip (<=) 2

        shrinkNum : String -> String
        shrinkNum s =
            if String.length s == 1 then
                "0"
            else
                String.dropRight 1 s

        ( sign, num ) =
            if String.startsWith "-" s then
                ( "-", String.dropLeft 1 s )
            else
                ( "", s )

        sepIndex =
            String.indexes "." num |> List.head

        ( i, f ) =
            case sepIndex of
                Nothing ->
                    ( num, "" )

                Just index ->
                    ( String.left index num, String.dropLeft (index + 1) num )

        s_ =
            if String.isEmpty f then
                sign ++ (shrinkNum i)
            else
                (sign ++ (shrinkNum i)) ++ ("." ++ (shrinkNum f))
    in
        if isMinimalCase s then
            empty
        else
            s_ ::: empty


join : Maybe (Maybe a) -> Maybe a
join mx =
    case mx of
        Just x ->
            x

        Nothing ->
            Nothing


trimTrailingZero : String -> String
trimTrailingZero =
    Regex.replace Regex.All (Regex.regex "(?<=[1-9])0*$") (\_ -> "")


suite : Test
suite =
    describe "Decimal module"
        [ describe "fromInt"
            [ fuzz maxIntRange "should create correct Decimal" <|
                \i ->
                    Expect.equal (i |> fromInt >> Decimal.toString) (Basics.toString i)
            ]
        , describe "fromInteger"
            [ fuzz intString "should create correct Decimal" <|
                \i ->
                    let
                        integer =
                            Integer.fromString i

                        decimal =
                            Maybe.map fromInteger integer
                    in
                        Expect.equal (Maybe.map Decimal.toString decimal) (Maybe.map Integer.toString integer)
            ]
        , describe "fromFloat"
            [ fuzz (floatRange (toFloat Random.minInt) (toFloat Random.maxInt)) "should create correct Decimal" <|
                \f ->
                    Expect.equal (f |> fromFloat >> Decimal.toString) (Basics.toString f)
            ]
        , describe "fromString"
            [ fuzz decimalString "should create correct Decimal" <|
                \d ->
                    Expect.equal (Maybe.map Decimal.toString << fromString <| d) (Just d)
            ]
        , describe "add"
            [ fuzz2 decimalString decimalString "should have transitivity property" <|
                \d1 d2 ->
                    let
                        a =
                            fromString d1

                        b =
                            fromString d2
                    in
                        Expect.equal (Maybe.map2 add a b) (Maybe.map2 add b a)
            , fuzz3 decimalString decimalString decimalString "should have associativity property" <|
                \d1 d2 d3 ->
                    let
                        a =
                            fromString d1

                        b =
                            fromString d2

                        c =
                            fromString d3
                    in
                        Expect.equal (Maybe.map2 add (Maybe.map2 add a b) c)
                            (Maybe.map2 add
                                a
                                (Maybe.map2 add b c)
                            )
            , fuzz decimalString "should have identity property" <|
                \d ->
                    let
                        a =
                            fromString d

                        b =
                            fromFloat 0
                    in
                        Expect.equal (Maybe.map (add b) a) a
            , fuzz3 decimalString decimalString decimalString "should have distributive property" <|
                \d1 d2 d3 ->
                    let
                        a =
                            fromString d1

                        b =
                            fromString d2

                        c =
                            fromString d3
                    in
                        Expect.equal (Maybe.map2 mul a (Maybe.map2 add b c))
                            (Maybe.map2 add
                                (Maybe.map2 mul a b)
                                (Maybe.map2 mul a c)
                            )
            ]
        , describe "sub"
            [ fuzz decimalString "should have identity property" <|
                \d ->
                    let
                        a =
                            fromString d

                        b =
                            Just (fromFloat 0)
                    in
                        Expect.equal (Maybe.map2 sub a b) a
            , fuzz decimalString "should have inverse" <|
                \d ->
                    let
                        a =
                            fromString d

                        b =
                            Just (fromInt 0)
                    in
                        Expect.equal (Maybe.map2 sub a a) b
            ]
        , describe "mul"
            [ fuzz2 decimalString decimalString "should have transitivity property" <|
                \d1 d2 ->
                    let
                        a =
                            fromString d1

                        b =
                            fromString d2
                    in
                        Expect.equal (Maybe.map2 mul a b) (Maybe.map2 mul b a)
            , fuzz3 decimalString decimalString decimalString "should have associativity property" <|
                \d1 d2 d3 ->
                    let
                        a =
                            fromString d1

                        b =
                            fromString d3

                        c =
                            fromString d3
                    in
                        Expect.equal (Maybe.map2 mul (Maybe.map2 mul a b) c)
                            (Maybe.map2 mul
                                a
                                (Maybe.map2 mul b c)
                            )
            , fuzz decimalString "should have identity property" <|
                \d ->
                    let
                        a =
                            fromString d

                        b =
                            fromFloat 1
                    in
                        Expect.equal (Maybe.map (mul b) a) a
            , fuzz3 decimalString decimalString decimalString "should have distributive property" <|
                \d1 d2 d3 ->
                    let
                        a =
                            fromString d1

                        b =
                            fromString d2

                        c =
                            fromString d3
                    in
                        Expect.equal (Maybe.map2 mul a (Maybe.map2 add b c))
                            (Maybe.map2 add
                                (Maybe.map2 mul a b)
                                (Maybe.map2 mul a c)
                            )
            ]
        , describe "div"
            [ fuzz decimalString "should have identity property" <|
                \d ->
                    let
                        a =
                            fromString d

                        b =
                            fromInt 1
                    in
                        Expect.equal (Maybe.andThen (flip div b) a) a
            , describe "should have zero-related properties"
                [ fuzz decimalString "dividing zero yields zero" <|
                    \d ->
                        let
                            a =
                                fromString d

                            b =
                                fromInt 0
                        in
                            Expect.equal (Maybe.andThen (div b) a) (Just b)
                , fuzz decimalString "dividing with zero yields Nothing" <|
                    \d ->
                        let
                            a =
                                fromString d

                            b =
                                fromInt 0
                        in
                            Expect.equal (Maybe.andThen (flip div b) a) Nothing
                ]
            , fuzz2 decimalString decimalString "should produce sufficiently precise result" <|
                \d1 d2 ->
                    let
                        a =
                            fromString d1

                        b =
                            fromString d2

                        maybeResult =
                            join (Maybe.map2 div a b)
                    in
                        case maybeResult of
                            Nothing ->
                                Expect.fail "was given invalid string for generating Decimal"

                            Just result ->
                                let
                                    product =
                                        Maybe.map (mul result) b
                                in
                                    if (result |> Decimal.toString >> String.length) < 20 then
                                        Expect.equal product a
                                    else
                                        -- TODO: add test for dynamic comparison for dynamic tolerance
                                        let
                                            diff =
                                                Maybe.map Decimal.abs
                                                    (Maybe.map2 sub a product)

                                            tolerance =
                                                Decimal.fromString ("0." ++ (String.repeat 1 "0") ++ "1")
                                        in
                                            Expect.pass
            ]
        , describe "abs"
            [ fuzz decimalString "abs f should be larger than or equal to f" <|
                \d ->
                    case (Maybe.map2 gte (Maybe.map Decimal.abs (fromString d)) (fromString d)) of
                        Just True ->
                            Expect.pass

                        _ ->
                            Expect.fail "property does not hold"
            ]
        , describe "negate"
            [ fuzz decimalString "should return original i when applied twice" <|
                \d ->
                    let
                        decimal =
                            fromString d

                        decimal_ =
                            decimal
                                |> Maybe.map (Decimal.negate >> Decimal.negate)
                    in
                        Expect.equal decimal decimal_
            ]
        , describe "compare"
            [ fuzz2 decimalString decimalString "should return correct order" <|
                \d1 d2 ->
                    let
                        a =
                            fromString d1

                        b =
                            fromString d2

                        comparison =
                            Maybe.map2 Decimal.compare a b
                    in
                        case comparison of
                            Nothing ->
                                Expect.fail "was given invalid string for generating Integer"

                            _ ->
                                Expect.equal (Maybe.map2 Decimal.compare (Maybe.map2 sub a b) (Just (fromInt 0))) comparison
            ]
        ]

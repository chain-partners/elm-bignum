module Test.Integer exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, intRange, custom)
import Integer exposing (..)
import Random.Pcg as Random exposing (Generator)
import Shrink exposing (Shrinker)
import Test exposing (..)
import Lazy.List exposing (empty, (:::))


intString : Fuzzer String
intString =
    custom intStringGenerator intStringShrinker


intStringGenerator : Generator String
intStringGenerator =
    let
        num =
            Random.choices
                (List.range 1 5 |> List.map numBuilder)

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
        Random.map2 (++) sign num


intStringShrinker : Shrinker String
intStringShrinker s =
    let
        baseInts =
            List.range -9 9
                |> List.map Basics.toString
    in
        if List.member s baseInts then
            empty
        else
            (String.dropRight 1 s) ::: empty


join : Maybe (Maybe a) -> Maybe a
join mx =
    case mx of
        Just x ->
            x

        Nothing ->
            Nothing


maxIntRange : Fuzzer Int
maxIntRange =
    intRange Random.minInt Random.maxInt


halfMaxIntRange : Fuzzer Int
halfMaxIntRange =
    intRange (Basics.negate (2 ^ 26)) (2 ^ 26)


nonZeroInt : Fuzzer Int
nonZeroInt =
    Fuzz.map
        (\i ->
            if i == 0 then
                1
            else
                i
        )
        maxIntRange


suite : Test
suite =
    describe "Integer module"
        [ describe "fromInt"
            [ fuzz maxIntRange "should have correct sign as Int" <|
                \i ->
                    Expect.equal (Integer.compare (fromInt i) (fromInt 0)) (Basics.compare i 0)
            , fuzz maxIntRange "should have same string representation as Int" <|
                \i ->
                    Expect.equal (Integer.toString << fromInt <| i) (Basics.toString i)
            , fuzz2 maxIntRange maxIntRange "should have same result for addition as Int" <|
                \i1 i2 ->
                    Expect.equal (add (fromInt i1) (fromInt i2)) (fromInt (i1 + i2))
            , fuzz2 maxIntRange maxIntRange "should have same result for subtraction as Int" <|
                \i1 i2 ->
                    Expect.equal (sub (fromInt i1) (fromInt i2)) (fromInt (i1 - i2))
            , fuzz2 halfMaxIntRange halfMaxIntRange "should have same result for multiplication as Int" <|
                \i1 i2 ->
                    Expect.equal (mul (fromInt i1) (fromInt i2)) (fromInt (i1 * i2))
            , fuzz2 maxIntRange maxIntRange "should have same result for division as Int" <|
                \i1 i2 ->
                    Expect.equal
                        (div (fromInt i1) (fromInt i2))
                        (Just (fromInt (i1 // i2)))
            , fuzz2 maxIntRange nonZeroInt "should have same result for divmod as Int" <|
                \i1 i2 ->
                    let
                        intDivmod =
                            Just ( fromInt (i1 // i2), fromInt (Basics.rem i1 i2) )

                        integerDivmod =
                            divmod (fromInt i1) (fromInt i2)
                    in
                        Expect.equal intDivmod integerDivmod
            ]
        , describe "fromString and toString"
            [ fuzz intString "should be inverse functions" <|
                \i -> Expect.equal ((Maybe.map Integer.toString) (fromString i)) (Just i)
            ]
        , describe "add"
            [ fuzz2 intString intString "should have transitivity property" <|
                \i1 i2 ->
                    let
                        a =
                            fromString i1

                        b =
                            fromString i2
                    in
                        Expect.equal (Maybe.map2 add a b) (Maybe.map2 add b a)
            , fuzz3 intString intString intString "should have associativity property" <|
                \i1 i2 i3 ->
                    let
                        a =
                            fromString i1

                        b =
                            fromString i2

                        c =
                            fromString i3
                    in
                        Expect.equal (Maybe.map2 add (Maybe.map2 add a b) c)
                            (Maybe.map2 add a (Maybe.map2 add b c))
            , fuzz intString "should have identity property" <|
                \i ->
                    let
                        a =
                            fromString i

                        b =
                            fromInt 0
                    in
                        Expect.equal (Maybe.map (add b) a) a
            , fuzz3 intString intString intString "should have distributive property" <|
                \i1 i2 i3 ->
                    let
                        a =
                            fromString i1

                        b =
                            fromString i2

                        c =
                            fromString i3
                    in
                        Expect.equal (Maybe.map2 mul a (Maybe.map2 add b c))
                            (Maybe.map2 add
                                (Maybe.map2 mul a b)
                                (Maybe.map2 mul a c)
                            )
            ]
        , describe "sub"
            [ fuzz intString "should have identity property" <|
                \i ->
                    let
                        a =
                            fromString i

                        b =
                            fromString "0"
                    in
                        Expect.equal (Maybe.map2 sub a b) a
            , fuzz intString "should have inverse" <|
                \i ->
                    let
                        a =
                            fromString i

                        b =
                            fromString "0"
                    in
                        Expect.equal (Maybe.map2 sub a a) b
            ]
        , describe "mul"
            [ fuzz2 intString intString "should have transitivity property" <|
                \i1 i2 ->
                    let
                        a =
                            fromString i1

                        b =
                            fromString i2
                    in
                        Expect.equal (Maybe.map2 mul a b) (Maybe.map2 mul b a)
            , fuzz3 intString intString intString "should have associativity property" <|
                \i1 i2 i3 ->
                    let
                        a =
                            fromString i1

                        b =
                            fromString i2

                        c =
                            fromString i3
                    in
                        Expect.equal (Maybe.map2 mul (Maybe.map2 mul a b) c)
                            (Maybe.map2 mul a (Maybe.map2 mul b c))
            , fuzz intString "should have identity property" <|
                \i ->
                    let
                        a =
                            fromString i

                        b =
                            fromInt 1
                    in
                        Expect.equal (Maybe.map (mul b) a) a
            , fuzz3 intString intString intString "should have distributive property" <|
                \i1 i2 i3 ->
                    let
                        a =
                            fromString i1

                        b =
                            fromString i2

                        c =
                            fromString i3
                    in
                        Expect.equal (Maybe.map2 mul a (Maybe.map2 add b c))
                            (Maybe.map2 add (Maybe.map2 mul a b) (Maybe.map2 mul a c))
            ]
        , describe "divmod"
            [ fuzz intString "should have identity property" <|
                \i ->
                    let
                        a =
                            fromString i

                        b =
                            fromInt 1

                        expected =
                            Maybe.map2 (,) a (fromString "0")
                    in
                        Expect.equal (Maybe.andThen (flip divmod b) a) expected
            , describe "should have zero-related properties"
                [ fuzz intString "dividing zero yields zero" <|
                    \i ->
                        let
                            a =
                                fromString i

                            b =
                                fromInt 0
                        in
                            Expect.equal (Maybe.andThen (divmod b) a) (Just ( b, b ))
                , fuzz intString "dividing with zero yields Nothing" <|
                    \i ->
                        let
                            a =
                                fromString i

                            b =
                                fromInt 0
                        in
                            Expect.equal (Maybe.andThen (flip divmod b) a) Nothing
                ]
            , fuzz2 intString intString "should produce valid quotient and remainder" <|
                \i1 i2 ->
                    let
                        a =
                            fromString i1

                        b =
                            fromString i2

                        result =
                            join (Maybe.map2 divmod a b)
                    in
                        case result of
                            Nothing ->
                                Expect.fail "was given invalid string for generating Integer"

                            Just ( q, r ) ->
                                Expect.equal (Maybe.map (add r) (Maybe.map (mul q) b)) a
            ]
        , describe "abs"
            [ fuzz intString "abs i should be larger than or equal to i" <|
                \i ->
                    case (Maybe.map2 gte (i |> fromString >> Maybe.map Integer.abs) (fromString i)) of
                        Just True ->
                            Expect.pass

                        _ ->
                            Expect.fail "property does not hold"
            ]
        , describe "negate"
            [ fuzz intString "should return original i when applied twice" <|
                \i ->
                    let
                        integer =
                            fromString i

                        integer_ =
                            integer
                                |> Maybe.map (Integer.negate >> Integer.negate)
                    in
                        Expect.equal integer integer_
            ]
        , describe "compare"
            [ fuzz2 intString intString "should return correct order" <|
                \i1 i2 ->
                    let
                        a =
                            fromString i1

                        b =
                            fromString i2

                        comparison =
                            Maybe.map2 Integer.compare a b
                    in
                        case comparison of
                            Nothing ->
                                Expect.fail "was given invalid string for generating Integer"

                            _ ->
                                Expect.equal
                                    (Maybe.map2 Integer.compare
                                        (Maybe.map2 sub a b)
                                        (fromString "0")
                                    )
                                    comparison
            ]
        ]

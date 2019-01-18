module Test.Integer exposing (fuzzer, maxIntRange, suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Integer exposing (Integer)
import Random exposing (Generator)
import Shrink exposing (Shrinker)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3)


fuzzer : Fuzzer Integer
fuzzer =
    Fuzz.custom generator shrinker


generator : Generator Integer
generator =
    Random.int 1 10
        |> Random.andThen (\i -> Random.list i (Random.int 0 Random.maxInt))
        |> Random.map (List.map String.fromInt >> List.foldl (++) "")
        |> Random.map2 (++) (Random.uniform "" [ "-" ])
        |> Random.map (Integer.fromString >> Maybe.withDefault Integer.zero)


shrinker : Shrinker Integer
shrinker i =
    if Integer.lt (Integer.abs i) Integer.ten then
        Shrink.noShrink i

    else
        Shrink.bool True
            |> Shrink.map (\_ -> Integer.div i Integer.ten |> Maybe.withDefault Integer.zero)


maxIntRange : Fuzzer Int
maxIntRange =
    Fuzz.intRange Random.minInt Random.maxInt


halfMaxIntRange : Fuzzer Int
halfMaxIntRange =
    Fuzz.intRange (Basics.negate (2 ^ 26)) (2 ^ 26)


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
            [ Test.fuzz maxIntRange "should have identical sign as Int" <|
                \i ->
                    Expect.equal (Integer.compare (Integer.fromInt i) (Integer.fromInt 0)) (Basics.compare i 0)
            , Test.fuzz maxIntRange "should have same string representation as Int" <|
                \i ->
                    Expect.equal (Integer.toString << Integer.fromInt <| i) (String.fromInt i)
            , Test.fuzz2 maxIntRange maxIntRange "should have same result for addition as Int" <|
                \i1 i2 ->
                    Expect.equal (Integer.add (Integer.fromInt i1) (Integer.fromInt i2)) (Integer.fromInt (i1 + i2))
            , Test.fuzz2 maxIntRange maxIntRange "should have same result for subtraction as Int" <|
                \i1 i2 ->
                    Expect.equal (Integer.sub (Integer.fromInt i1) (Integer.fromInt i2)) (Integer.fromInt (i1 - i2))
            , Test.fuzz2 halfMaxIntRange halfMaxIntRange "should have same result for multiplication as Int" <|
                \i1 i2 ->
                    Expect.equal (Integer.mul (Integer.fromInt i1) (Integer.fromInt i2)) (Integer.fromInt (i1 * i2))
            , Test.fuzz2 maxIntRange maxIntRange "should have same result for division as Int" <|
                \i1 i2 ->
                    Expect.equal
                        (Integer.div (Integer.fromInt i1) (Integer.fromInt i2))
                        (Just (Integer.fromInt (i1 // i2)))
            , Test.fuzz2 maxIntRange nonZeroInt "should have same result for divmod as Int" <|
                \i1 i2 ->
                    let
                        intDivmod =
                            Just ( Integer.fromInt (i1 // i2), Integer.fromInt (Basics.remainderBy i2 i1) )

                        integerDivmod =
                            Integer.divmod (Integer.fromInt i1) (Integer.fromInt i2)
                    in
                    Expect.equal intDivmod integerDivmod
            ]
        , describe "fromString and toString"
            [ Test.fuzz fuzzer "should be inverse functions" <|
                \i -> Expect.equal (Integer.fromString (Integer.toString i)) (Just i)
            ]
        , describe "add"
            [ Test.fuzz2 fuzzer fuzzer "should have transitivity property" <|
                \i1 i2 ->
                    Expect.equal (Integer.add i1 i2) (Integer.add i2 i1)
            , Test.fuzz3 fuzzer fuzzer fuzzer "should have associativity property" <|
                \i1 i2 i3 ->
                    Expect.equal (Integer.add (Integer.add i1 i2) i3) (Integer.add i1 (Integer.add i2 i3))
            , Test.fuzz fuzzer "should have identity property" <|
                \i ->
                    Expect.equal (Integer.add Integer.zero i) i
            , Test.fuzz3 fuzzer fuzzer fuzzer "should have distributive property" <|
                \i1 i2 i3 ->
                    Expect.equal (Integer.mul i1 (Integer.add i2 i3)) (Integer.add (Integer.mul i1 i2) (Integer.mul i1 i3))
            ]
        , describe "sub"
            [ Test.fuzz fuzzer "should have identity property" <|
                \i ->
                    Expect.equal (Integer.sub i Integer.zero) i
            , Test.fuzz fuzzer "should have inverse" <|
                \i ->
                    Expect.equal (Integer.sub i i) Integer.zero
            ]
        , describe "mul"
            [ Test.fuzz2 fuzzer fuzzer "should have transitivity property" <|
                \i1 i2 ->
                    Expect.equal (Integer.mul i1 i2) (Integer.mul i2 i1)
            , Test.fuzz3 fuzzer fuzzer fuzzer "should have associativity property" <|
                \i1 i2 i3 ->
                    Expect.equal (Integer.mul (Integer.mul i1 i2) i3) (Integer.mul i1 (Integer.mul i2 i3))
            , Test.fuzz fuzzer "should have identity property" <|
                \i ->
                    Expect.equal (Integer.mul i Integer.one) i
            , Test.fuzz3 fuzzer fuzzer fuzzer "should have distributive property" <|
                \i1 i2 i3 ->
                    Expect.equal (Integer.mul i1 (Integer.add i2 i3)) (Integer.add (Integer.mul i1 i2) (Integer.mul i1 i3))
            ]
        , describe "divmod"
            [ Test.fuzz fuzzer "should have identity property" <|
                \i ->
                    Expect.equal (Integer.divmod i Integer.one) (Just ( i, Integer.zero ))
            , describe "should have zero-related properties"
                [ Test.fuzz fuzzer "dividing zero yields zero" <|
                    \i ->
                        Expect.equal (Integer.divmod Integer.zero i) (Just ( Integer.zero, Integer.zero ))
                , Test.fuzz fuzzer "dividing with zero yields Nothing" <|
                    \i ->
                        Expect.equal (Integer.divmod i Integer.zero) Nothing
                ]
            , Test.fuzz2 fuzzer fuzzer "should produce valid quotient and remainder" <|
                \i1 i2 ->
                    let
                        result =
                            Integer.divmod i1 i2
                    in
                    case result of
                        Nothing ->
                            Expect.fail "was given invalid string for generating Integer"

                        Just ( q, r ) ->
                            Expect.equal (Integer.add r (Integer.mul q i2)) i1
            ]
        , describe "abs"
            [ Test.fuzz fuzzer "abs i should be larger than or equal to i" <|
                \i ->
                    if Integer.gte (Integer.abs i) i then
                        Expect.pass

                    else
                        Expect.fail "property does not hold"
            ]
        , describe "negate"
            [ Test.fuzz fuzzer "should return original i when applied twice" <|
                \i ->
                    let
                        i_ =
                            Integer.negate << Integer.negate <| i
                    in
                    Expect.equal i i_
            ]
        , describe "compare"
            [ Test.fuzz2 fuzzer fuzzer "should return correct order" <|
                \i1 i2 ->
                    Expect.equal
                        (Integer.compare (Integer.sub i1 i2) Integer.zero)
                        (Integer.compare i1 i2)
            ]
        ]

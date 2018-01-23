module Test.Integer exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string, intRange)
import Random.Pcg as Random
import Test exposing (..)
import Integer exposing (..)


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
                        (safeDiv (fromInt i1)
                            (fromInt i2)
                        )
                        (Just (fromInt (i1 // i2)))
            , fuzz2 maxIntRange nonZeroInt "should have same result for divmod as Int" <|
                \i1 i2 ->
                    let
                        intDivmod =
                            Just ( fromInt (i1 // i2), fromInt (Basics.rem i1 i2) )

                        integerDivmod =
                            safeDivmod (fromInt i1) (fromInt i2)
                    in
                        Expect.equal intDivmod integerDivmod
            ]
        , describe "add"
            [ test "should have transitivity property" <|
                \_ ->
                    let
                        a =
                            fromInt (2 ^ 50 - 7128937)

                        b =
                            fromInt (2 ^ 20 + 17171111)
                    in
                        Expect.equal (add a b) (add b a)
            , test "should have associativity property" <|
                \_ ->
                    let
                        a =
                            fromInt (2 ^ 50 - 25222523)

                        b =
                            fromInt (2 ^ 49 + 123134)

                        c =
                            fromInt (2 ^ 16 - 77859)
                    in
                        Expect.equal (add (add a b) c) (add a (add b c))
            , test "should have identity property" <|
                \_ ->
                    let
                        a =
                            fromInt (2 ^ 48 + 812381)

                        b =
                            fromInt 0
                    in
                        Expect.equal (add a b) a
            , test "should have distributive property" <|
                \_ ->
                    let
                        a =
                            fromInt (2 ^ 36 - 188111)

                        b =
                            fromInt (Basics.negate (2 ^ 15 - 7333))

                        c =
                            fromInt (2 ^ 22 + 7221)
                    in
                        Expect.equal (mul a (add b c)) (add (mul a b) (mul a c))
            ]
        , describe "sub"
            [ test "should have identity property" <|
                \_ ->
                    let
                        a =
                            (fromInt (Basics.negate (2 ^ 50 + 32)))

                        b =
                            fromInt 0
                    in
                        Expect.equal (sub a b) a
            , test "should have inverse" <|
                \_ ->
                    let
                        a =
                            fromInt ((2 ^ 44) * 3)

                        b =
                            fromInt 0
                    in
                        Expect.equal (sub a a) b
            ]
        , describe "mul"
            [ test "should have transitivity property" <|
                \_ ->
                    let
                        a =
                            fromInt (2 ^ 50 - 7128937)

                        b =
                            fromInt (2 ^ 20 + 17171111)
                    in
                        Expect.equal (mul a b) (mul b a)
            , test "should have associativity property" <|
                \_ ->
                    let
                        a =
                            fromInt (2 ^ 50 - 25222523)

                        b =
                            fromInt (2 ^ 49 + 123134)

                        c =
                            fromInt (2 ^ 16 - 77859)
                    in
                        Expect.equal (mul (mul a b) c) (mul a (mul b c))
            , test "should have identity property" <|
                \_ ->
                    let
                        a =
                            fromInt (2 ^ 48 + 812381)

                        b =
                            fromInt 1
                    in
                        Expect.equal (mul a b) a
            , test "should have distributive property" <|
                \_ ->
                    let
                        a =
                            fromInt (2 ^ 36 - 188111)

                        b =
                            fromInt (Basics.negate (2 ^ 15 - 7333))

                        c =
                            fromInt (2 ^ 22 + 7221)
                    in
                        Expect.equal (mul a (add b c)) (add (mul a b) (mul a c))
            ]
        , describe "div"
            [ test "should have identity property" <|
                \_ ->
                    let
                        a =
                            fromInt (2 ^ 50 - 7128937)

                        b =
                            fromInt 1
                    in
                        Expect.equal (div a b) a
            , describe "should have zero property"
                [ test "dividing zero yields zero" <|
                    \_ ->
                        let
                            a =
                                fromInt (2 ^ 35 + 8172811)

                            b =
                                fromInt 0
                        in
                            Expect.equal (div b a) b
                , test "dividing with zero yields Nothing" <|
                    \_ ->
                        let
                            a =
                                fromInt (2 ^ 35 + 4113)

                            b =
                                fromInt 0
                        in
                            Expect.equal (safeDiv a b) Nothing
                ]
            , describe "should have distributive property"
                [ test "should have left-associative distributive property" <|
                    \_ ->
                        let
                            a =
                                fromInt (2 ^ 50 - 1211223)

                            b =
                                fromInt (2 ^ 33 + 12311)

                            c =
                                fromInt (2 ^ 20 - 997831)

                            ( aq, ar ) =
                                divmod a c

                            ( bq, br ) =
                                divmod b c

                            sumq =
                                add aq bq

                            sumr =
                                add ar br

                            ( sumr_carry, sumr_r ) =
                                divmod sumr c
                        in
                            Expect.equal (divmod (add a b) c) ( add sumq sumr_carry, sumr_r )
                , test "should not have right-associative distributive property" <|
                    \_ ->
                        let
                            a =
                                fromInt (2 ^ 20 + 3335)

                            b =
                                fromInt (2 ^ 25 - 8311)

                            c =
                                fromInt (2 ^ 5 + 3)

                            ( acq, acr ) =
                                divmod a c

                            ( abq, abr ) =
                                divmod a b

                            ( q, r ) =
                                divmod a (add b c)

                            sumq =
                                add acq abq

                            sumr =
                                add abr acr

                            ( sumr_carry, sumr_r ) =
                                divmod sumr a
                        in
                            Expect.notEqual ( q, r ) ( add sumq sumr_carry, sumr_r )
                ]
            ]
        ]

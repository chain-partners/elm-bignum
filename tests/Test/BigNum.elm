module Test.BigNum exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import BigNum exposing (..)


suite : Test
suite =
    describe "BigNum module"
        [ describe "add"
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
                        Expect.equal (unsafeDiv a b) a
            , describe "should have zero property"
                [ test "dividing zero yields zero" <|
                    \_ ->
                        let
                            a =
                                fromInt (2 ^ 35 + 8172811)

                            b =
                                fromInt 0
                        in
                            Expect.equal (unsafeDiv b a) b
                , test "dividing with zero yields Nothing" <|
                    \_ ->
                        let
                            a =
                                fromInt (2 ^ 35 + 4113)

                            b =
                                fromInt 0
                        in
                            Expect.equal (div a b) Nothing
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
                                Debug.log "aq ar" (unsafeDivmod a c)

                            ( bq, br ) =
                                Debug.log "bq br" (unsafeDivmod b c)
                        in
                            Expect.equal (unsafeDivmod (add a b) c) ( add aq bq, add ar br )
                ]
            ]
        ]

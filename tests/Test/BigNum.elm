module Test.BigNum exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import BigNum exposing (..)


suite : Test
suite =
    describe "add"
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

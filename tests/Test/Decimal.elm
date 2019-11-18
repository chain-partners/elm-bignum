module Test.Decimal exposing (suite)

import Char
import Decimal exposing (Decimal, Exponent)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Integer
import Random exposing (Generator)
import Regex
import Shrink exposing (Shrinker)
import Test exposing (Test, describe, fuzz, fuzz2, fuzz3, test)
import Test.Integer


fuzzer : Fuzzer Decimal
fuzzer =
    Fuzz.oneOf [ sciDecimalFuzzer, decDecimalFuzzer ]


sciDecimalFuzzer : Fuzzer Decimal
sciDecimalFuzzer =
    Fuzz.custom (Random.map (Decimal.fromString >> Maybe.withDefault (Decimal.fromInt 0)) scientificStringGenerator) shrinker


decDecimalFuzzer : Fuzzer Decimal
decDecimalFuzzer =
    Fuzz.custom (Random.map (Decimal.fromString >> Maybe.withDefault (Decimal.fromInt 0)) decimalStringGenerator) shrinker


decimalStringGenerator : Generator String
decimalStringGenerator =
    let
        int =
            Random.int 1 5
                |> Random.andThen (\i -> Random.list i (Random.int 0 Random.maxInt))
                |> Random.map (List.map String.fromInt >> List.foldl (++) "")

        fraction =
            Random.int 0 3
                |> Random.andThen (\i -> Random.list i (Random.int 0 Random.maxInt))
                |> Random.map (List.map String.fromInt >> List.foldl (++) "")

        sign =
            Random.uniform "" [ "-" ]
    in
    Random.map2 (++) (Random.constant ".") fraction
        |> Random.map2 (++) int
        |> Random.map2 (++) sign


scientificStringGenerator : Generator String
scientificStringGenerator =
    let
        i =
            Random.int 0 9 |> Random.map String.fromInt

        f =
            Random.int Random.minInt Random.maxInt
                |> Random.map
                    (String.fromInt
                        >> (\int ->
                                if int == "0" then
                                    ""

                                else
                                    "." ++ int
                           )
                    )

        coefficient =
            Random.map2 (++) i f

        exponent =
            Random.int Decimal.minExponent (Basics.negate Decimal.minExponent) |> Random.map (String.fromInt >> (++) "e")
    in
    Random.map2 (++) coefficient exponent


shrinker : Shrinker Decimal
shrinker d =
    let
        e =
            Decimal.exponent d

        e_ =
            if e >= 0 then
                e

            else
                Basics.min 0 (e + 2)

        d_ =
            Decimal.div d (Decimal.fromInt 10)
                |> Maybe.map (Decimal.roundWithContext { e = e_, mode = Decimal.Down })
                |> Maybe.withDefault (Decimal.fromInt 0)
    in
    if Decimal.eq d (Decimal.fromInt 0) || Decimal.eq d d_ then
        Shrink.noShrink d

    else
        Shrink.bool True
            |> Shrink.map (\_ -> d_)


trimTrailingZero : String -> String
trimTrailingZero s =
    if String.contains "." s && (String.endsWith "0" s || String.endsWith "." s) then
        trimTrailingZero (String.dropRight 1 s)

    else
        s



{--
`Decimal.div` is tested using `withinTolerance` function, which asserts that `| a - (Decimal.div a b |> Decimal.mul b) | <= 0.01 * a`. This test fails when the quotient has only a few significant digits, i.e. 0.000000000000032, because of computer's precision issue. `preciseDiv` function addresses this issue by calculating quotient with at least 8 significant digits.
--}


preciseDiv : Exponent -> Decimal -> Decimal -> Maybe Decimal
preciseDiv e d1 d2 =
    case Decimal.divToMinE e d1 d2 of
        Nothing ->
            Nothing

        Just Decimal.Zero ->
            Just Decimal.Zero

        Just result ->
            if result |> Decimal.significand |> Integer.countDigits |> (\i -> i > 7 || i == 0) then
                Just result

            else
                preciseDiv (e - 8) d1 d2


withinTolerance : Decimal -> Decimal -> Expectation
withinTolerance d1 d2 =
    let
        tolerance =
            Decimal.fromFloat 0.01

        diff =
            Decimal.sub d1 d2

        diffBound1 =
            Decimal.abs (Decimal.mul d1 tolerance)

        diffBound2 =
            Decimal.abs (Decimal.mul d2 tolerance)

        predicate1 =
            Decimal.gte diff (Decimal.negate diffBound1)
                && Decimal.lte diff diffBound1

        predicate2 =
            Decimal.gte diff (Decimal.negate diffBound2)
                && Decimal.lte diff diffBound2
    in
    if Decimal.eq diff (Decimal.fromInt 0) || predicate1 || predicate2 then
        Expect.pass

    else
        Expect.fail ("Difference between values is: " ++ Decimal.toString diff ++ ". The difference should be one of the following: 0, between " ++ (diffBound1 |> Decimal.negate >> Decimal.toString) ++ " and " ++ (diffBound1 |> Decimal.toString) ++ ", or between " ++ (diffBound2 |> Decimal.negate >> Decimal.toString) ++ " and " ++ (diffBound2 |> Decimal.toString) ++ ".")


sqrtValues : List ( Int, String )
sqrtValues =
    [ ( 2, "1.41421356237309504880168872420969807856967187537694807317667973799073247846" )
    , ( 3, "1.73205080756887729352744634150587236694280525381038062805580697945193301690" )
    , ( 5, "2.23606797749978969640917366873127623544061835961152572427089724541052092563" )
    , ( 6, "2.44948974278317809819728407470589139196594748065667012843269256725096037745" )
    , ( 7, "2.64575131106459059050161575363926042571025918308245018036833445920106882323" )
    , ( 8, "2.82842712474619009760337744841939615713934375075389614635335947598146495692" )
    , ( 10, "3.16227766016837933199889354443271853371955513932521682685750485279259443863" )
    , ( 11, "3.31662479035539984911493273667068668392708854558935359705868214611648464260" )
    , ( 12, "3.46410161513775458705489268301174473388561050762076125611161395890386603381" )
    , ( 13, "3.60555127546398929311922126747049594625129657384524621271045305622716694829" )
    , ( 14, "3.74165738677394138558374873231654930175601980777872694630374546732003515630" )
    , ( 15, "3.87298334620741688517926539978239961083292170529159082658757376611348309193" )
    , ( 17, "4.12310562561766054982140985597407702514719922537362043439863357309495434633" )
    , ( 18, "4.24264068711928514640506617262909423570901562613084421953003921397219743538" )
    , ( 19, "4.35889894354067355223698198385961565913700392523244493689034413815955732820" )
    , ( 20, "4.47213595499957939281834733746255247088123671922305144854179449082104185127" )
    , ( 21, "4.58257569495584000658804719372800848898445657676797190260724212390686842554" )
    ]


suite : Test
suite =
    describe "Decimal module"
        [ describe "fromInt"
            [ fuzz Test.Integer.maxIntRange "should create correct Decimal" <|
                \i ->
                    Expect.equal (i |> Decimal.fromInt >> Decimal.toString) (String.fromInt i)
            ]
        , describe "fromInteger"
            [ fuzz Test.Integer.fuzzer "should create correct Decimal" <|
                \i ->
                    Expect.equal (i |> Decimal.fromInteger |> Decimal.toString) (Integer.toString i)
            ]
        , describe "fromFloat"
            [ fuzz (Fuzz.floatRange (toFloat Random.minInt) (toFloat Random.maxInt)) "should create correct Decimal" <|
                \f ->
                    Expect.equal (f |> Decimal.fromFloat >> Decimal.toString) (String.fromFloat f)
            ]
        , describe "fromString"
            [ fuzz fuzzer "should create correct Decimal" <|
                \d ->
                    Maybe.map2 withinTolerance (d |> Decimal.toString >> Decimal.fromString) (Just d) |> Maybe.withDefault (Expect.fail "failed")
            ]
        , describe "add"
            [ fuzz2 fuzzer fuzzer "should have transitivity property" <|
                \d1 d2 ->
                    withinTolerance (Decimal.add d1 d2) (Decimal.add d2 d1)
            , fuzz3 fuzzer fuzzer fuzzer "should have associativity property" <|
                \d1 d2 d3 ->
                    withinTolerance (Decimal.add (Decimal.add d1 d2) d3) (Decimal.add d1 (Decimal.add d2 d3))
            , fuzz fuzzer "should have identity property" <|
                \d ->
                    withinTolerance (Decimal.add (Decimal.fromFloat 0) d) d
            , fuzz3 fuzzer fuzzer fuzzer "should have distributive property" <|
                \d1 d2 d3 ->
                    withinTolerance
                        (Decimal.mul d1 (Decimal.add d2 d3))
                        (Decimal.add (Decimal.mul d1 d2) (Decimal.mul d1 d3))
            ]
        , describe "sub"
            [ fuzz fuzzer "should have identity property" <|
                \d ->
                    withinTolerance (Decimal.sub d (Decimal.fromFloat 0)) d
            , fuzz fuzzer "should have inverse" <|
                \d ->
                    withinTolerance (Decimal.sub d d) (Decimal.fromInt 0)
            ]
        , describe "mul"
            [ fuzz2 fuzzer fuzzer "should have transitivity property" <|
                \d1 d2 ->
                    withinTolerance (Decimal.mul d1 d2) (Decimal.mul d2 d1)
            , fuzz3 fuzzer fuzzer fuzzer "should have associativity property" <|
                \d1 d2 d3 ->
                    withinTolerance
                        (Decimal.mulToMinE (Decimal.minExponent * 2) (Decimal.mulToMinE (Decimal.minExponent * 2) d1 d2) d3)
                        (Decimal.mulToMinE (Decimal.minExponent * 2) d1 (Decimal.mulToMinE (Decimal.minExponent * 2) d2 d3))
            , fuzz fuzzer "should have identity property" <|
                \d ->
                    withinTolerance (Decimal.mul (Decimal.fromFloat 1) d) d
            , fuzz3 fuzzer fuzzer fuzzer "should have distributive property" <|
                \d1 d2 d3 ->
                    withinTolerance
                        (Decimal.mulToMinE (Decimal.minExponent * 2) d1 (Decimal.add d2 d3))
                        (Decimal.add (Decimal.mulToMinE (Decimal.minExponent * 2) d1 d2) (Decimal.mulToMinE (Decimal.minExponent * 2) d1 d3))
            ]
        , describe "div"
            [ fuzz fuzzer "should have identity property" <|
                \d ->
                    Maybe.map2 withinTolerance (Decimal.div d (Decimal.fromInt 1)) (Just d) |> Maybe.withDefault (Expect.fail "failed")
            , describe "should have zero-related properties"
                [ fuzz fuzzer "dividing zero yields zero" <|
                    \d ->
                        let
                            result =
                                Decimal.div (Decimal.fromInt 0) d
                        in
                        if Decimal.eq d (Decimal.fromInt 0) then
                            Expect.equal result Nothing

                        else
                            Expect.equal result (Just (Decimal.fromInt 0))
                , fuzz fuzzer "dividing with zero yields Nothing" <|
                    \d ->
                        Expect.equal
                            (Decimal.div d (Decimal.fromInt 0))
                            Nothing
                ]
            , fuzz2 fuzzer fuzzer "should produce sufficiently precise result" <|
                \d1 d2 ->
                    case preciseDiv Decimal.minExponent d1 d2 of
                        Nothing ->
                            if Decimal.eq d2 (Decimal.fromInt 0) then
                                Expect.pass

                            else
                                Expect.fail "was given invalid string for generating Decimal"

                        Just result ->
                            if Decimal.eq result (Decimal.fromInt 0) then
                                Expect.pass

                            else
                                withinTolerance (Decimal.mul result d2) d1
            ]
        , describe "abs"
            [ fuzz fuzzer "abs d should be larger than or equal to d" <|
                \d ->
                    Expect.true "Expected abs value to be gte itself" (Decimal.gte (Decimal.abs d) d)
            ]
        , describe "negate"
            [ fuzz fuzzer "should return original i when applied twice" <|
                \d ->
                    withinTolerance d (d |> Decimal.negate >> Decimal.negate)
            ]
        , describe "compare"
            [ fuzz2 fuzzer fuzzer "should return correct order" <|
                \d1 d2 ->
                    Expect.equal (Decimal.compare (Decimal.sub d1 d2) (Decimal.fromInt 0)) (Decimal.compare d1 d2)
            ]
        , describe "rounding"
            [ describe "down"
                [ test "1.8 equals 1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Down } (Decimal.fromFloat 1.8)) (Decimal.fromInt 1)
                , test "1.5 equals 1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Down } (Decimal.fromFloat 1.5)) (Decimal.fromInt 1)
                , test "1.2 equals 1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Down } (Decimal.fromFloat 1.2)) (Decimal.fromInt 1)
                , test "0.8 equals 0" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Down } (Decimal.fromFloat 0.8)) (Decimal.fromInt 0)
                , test "0.5 equals 0" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Down } (Decimal.fromFloat 0.5)) (Decimal.fromInt 0)
                , test "0.2 equals 0" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Down } (Decimal.fromFloat 0.2)) (Decimal.fromInt 0)
                , test "-0.2 equals -1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Down } (Decimal.fromFloat -0.2)) (Decimal.fromInt -1)
                , test "-0.5 equals -1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Down } (Decimal.fromFloat -0.5)) (Decimal.fromInt -1)
                , test "-0.8 equals -1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Down } (Decimal.fromFloat -0.8)) (Decimal.fromInt -1)
                , test "-1.2 equals -2" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Down } (Decimal.fromFloat -1.2)) (Decimal.fromInt -2)
                , test "-1.5 equals -2" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Down } (Decimal.fromFloat -1.5)) (Decimal.fromInt -2)
                , test "-1.8 equals -2" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Down } (Decimal.fromFloat -1.8)) (Decimal.fromInt -2)
                ]
            , describe "up"
                [ test "1.8 equals 2" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Up } (Decimal.fromFloat 1.8)) (Decimal.fromInt 2)
                , test "1.5 equals 2" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Up } (Decimal.fromFloat 1.5)) (Decimal.fromInt 2)
                , test "1.2 equals 2" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Up } (Decimal.fromFloat 1.2)) (Decimal.fromInt 2)
                , test "0.8 equals 1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Up } (Decimal.fromFloat 0.8)) (Decimal.fromInt 1)
                , test "0.5 equals 1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Up } (Decimal.fromFloat 0.5)) (Decimal.fromInt 1)
                , test "0.2 equals 1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Up } (Decimal.fromFloat 0.2)) (Decimal.fromInt 1)
                , test "-0.2 equals 0" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Up } (Decimal.fromFloat -0.2)) (Decimal.fromInt 0)
                , test "-0.5 equals 0" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Up } (Decimal.fromFloat -0.5)) (Decimal.fromInt 0)
                , test "-0.8 equals 0" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Up } (Decimal.fromFloat -0.8)) (Decimal.fromInt 0)
                , test "-1.2 equals -1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Up } (Decimal.fromFloat -1.2)) (Decimal.fromInt -1)
                , test "-1.5 equals -1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Up } (Decimal.fromFloat -1.5)) (Decimal.fromInt -1)
                , test "-1.8 equals -1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.Up } (Decimal.fromFloat -1.8)) (Decimal.fromInt -1)
                ]
            , describe "towards zero"
                [ test "1.8 equals 1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.TowardsZero } (Decimal.fromFloat 1.8)) (Decimal.fromInt 1)
                , test "1.5 equals 1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.TowardsZero } (Decimal.fromFloat 1.5)) (Decimal.fromInt 1)
                , test "1.2 equals 1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.TowardsZero } (Decimal.fromFloat 1.2)) (Decimal.fromInt 1)
                , test "0.8 equals 0" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.TowardsZero } (Decimal.fromFloat 0.8)) (Decimal.fromInt 0)
                , test "0.5 equals 0" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.TowardsZero } (Decimal.fromFloat 0.5)) (Decimal.fromInt 0)
                , test "0.2 equals 0" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.TowardsZero } (Decimal.fromFloat 0.2)) (Decimal.fromInt 0)
                , test "-0.2 equals 0" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.TowardsZero } (Decimal.fromFloat -0.2)) (Decimal.fromInt 0)
                , test "-0.5 equals 0" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.TowardsZero } (Decimal.fromFloat -0.5)) (Decimal.fromInt 0)
                , test "-0.8 equals 0" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.TowardsZero } (Decimal.fromFloat -0.8)) (Decimal.fromInt 0)
                , test "-1.2 equals -2" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.TowardsZero } (Decimal.fromFloat -1.2)) (Decimal.fromInt -1)
                , test "-1.5 equals -2" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.TowardsZero } (Decimal.fromFloat -1.5)) (Decimal.fromInt -1)
                , test "-1.8 equals -2" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.TowardsZero } (Decimal.fromFloat -1.8)) (Decimal.fromInt -1)
                ]
            , describe "away from zero"
                [ test "1.8 equals 2" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.AwayFromZero } (Decimal.fromFloat 1.8)) (Decimal.fromInt 2)
                , test "1.5 equals 2" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.AwayFromZero } (Decimal.fromFloat 1.5)) (Decimal.fromInt 2)
                , test "1.2 equals 2" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.AwayFromZero } (Decimal.fromFloat 1.2)) (Decimal.fromInt 2)
                , test "0.8 equals 1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.AwayFromZero } (Decimal.fromFloat 0.8)) (Decimal.fromInt 1)
                , test "0.5 equals 1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.AwayFromZero } (Decimal.fromFloat 0.5)) (Decimal.fromInt 1)
                , test "0.2 equals 1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.AwayFromZero } (Decimal.fromFloat 0.2)) (Decimal.fromInt 1)
                , test "-0.2 equals -1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.AwayFromZero } (Decimal.fromFloat -0.2)) (Decimal.fromInt -1)
                , test "-0.5 equals -1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.AwayFromZero } (Decimal.fromFloat -0.5)) (Decimal.fromInt -1)
                , test "-0.8 equals -1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.AwayFromZero } (Decimal.fromFloat -0.8)) (Decimal.fromInt -1)
                , test "-1.2 equals -2" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.AwayFromZero } (Decimal.fromFloat -1.2)) (Decimal.fromInt -2)
                , test "-1.5 equals -2" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.AwayFromZero } (Decimal.fromFloat -1.5)) (Decimal.fromInt -2)
                , test "-1.8 equals -2" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.AwayFromZero } (Decimal.fromFloat -1.8)) (Decimal.fromInt -2)
                ]
            , describe "half to even"
                [ test "1.8 equals 2" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.HalfToEven } (Decimal.fromFloat 1.8)) (Decimal.fromInt 2)
                , test "1.5 equals 2" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.HalfToEven } (Decimal.fromFloat 1.5)) (Decimal.fromInt 2)
                , test "1.2 equals 1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.HalfToEven } (Decimal.fromFloat 1.2)) (Decimal.fromInt 1)
                , test "0.8 equals 1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.HalfToEven } (Decimal.fromFloat 0.8)) (Decimal.fromInt 1)
                , test "0.5 equals 0" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.HalfToEven } (Decimal.fromFloat 0.5)) (Decimal.fromInt 0)
                , test "0.2 equals 0" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.HalfToEven } (Decimal.fromFloat 0.2)) (Decimal.fromInt 0)
                , test "-0.2 equals 0" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.HalfToEven } (Decimal.fromFloat -0.2)) (Decimal.fromInt 0)
                , test "-0.5 equals 0" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.HalfToEven } (Decimal.fromFloat -0.5)) (Decimal.fromInt 0)
                , test "-0.8 equals -1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.HalfToEven } (Decimal.fromFloat -0.8)) (Decimal.fromInt -1)
                , test "-1.2 equals -1" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.HalfToEven } (Decimal.fromFloat -1.2)) (Decimal.fromInt -1)
                , test "-1.5 equals -2" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.HalfToEven } (Decimal.fromFloat -1.5)) (Decimal.fromInt -2)
                , test "-1.8 equals -2" <|
                    \_ ->
                        Expect.equal (Decimal.roundWithContext { e = 0, mode = Decimal.HalfToEven } (Decimal.fromFloat -1.8)) (Decimal.fromInt -2)
                ]
            ]
        , describe "sqrt" <|
            List.map
                (\( i, sqrtValue ) ->
                    test ("sqrt of " ++ String.fromInt i) <|
                        \_ ->
                            case i |> Decimal.fromInt >> Decimal.sqrtToMinE -74 >> Maybe.map (Decimal.toString >> String.startsWith (sqrtValue |> String.left 74)) of
                                Just bool ->
                                    Expect.true "is correct up to 74 digits" bool

                                Nothing ->
                                    Expect.fail "could not be calculated"
                )
                sqrtValues
        ]

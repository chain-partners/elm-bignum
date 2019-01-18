module Decimal exposing
    ( Decimal(..)
    , Significand
    , Exponent
    , minExponent
    , fromInt
    , fromFloat
    , fromString
    , fromInteger
    , significand
    , exponent
    , add
    , sub
    , mul
    , mulToMinE
    , div
    , divToMinE
    , Notation(..)
    , toString
    , toStringIn
    , negate
    , abs
    , compare
    , lt
    , gt
    , lte
    , gte
    , eq
    , round
    , roundTo
    , floor
    , ceiling
    , truncate
    , roundWithContext
    , RoundingContext
    , RoundingMode(..)
    , sqrt
    , sqrtToMinE
    )

{-| This library provides arbitrary precision 'Decimal' type and basic arithmetic operations on it.


# Definition

@docs Decimal
@docs Significand
@docs Exponent
@docs minExponent


# Constructors

`Decimal` can be constructed from `Int`, `Float`, `String`, or `Integer`.

@docs fromInt
@docs fromFloat
@docs fromString
@docs fromInteger


# Deconstructors

@docs significand
@docs exponent


# Basic Arithmetic

@docs add
@docs sub
@docs mul
@docs mulToMinE
@docs div
@docs divToMinE


# String Representation

@docs Notation
@docs toString
@docs toStringIn


# Sign Modification

@docs negate
@docs abs


# Comparison

@docs compare
@docs lt
@docs gt
@docs lte
@docs gte
@docs eq


# Rounding

@docs round
@docs roundTo
@docs floor
@docs ceiling
@docs truncate
@docs roundWithContext
@docs RoundingContext
@docs RoundingMode


# Square Root

@docs sqrt
@docs sqrtToMinE

-}

import Char
import Integer as Integer exposing (Integer)
import Regex exposing (Regex)



-- Types


type alias Significand =
    Integer


type alias Exponent =
    Int


{-| A `Decimal` is an arbitrary precision number where its significand is represented as an `Integer`, and its exponent is represented as `Int`.
-}
type Decimal
    = Decimal Significand Exponent
    | Zero



-- Constants


{-| `minExponent` specifies the default minimum exponent. It's currently -32, chosen arbitrarily.
-}
minExponent : Int
minExponent =
    -32



-- Constructors


{-| Construct `Decimal` from `Int`.

    fromInt 89928271371 -- Decimal (Integer Positive [8271371,8992]) 0 : Decimal

    fromInt 0 -- Decimal Integer.Zero 0 : Decimal

    fromInt -899210300 -- Decimal (Integer Negative [8992103]) 2 : Decimal

    Entering numbers that cannot be represented by Javascript number type will result in incorrect representation. Use fromString in that case.

    fromInt 81927398127398127389172893712893712893798123 |> toString -- "-10497034289617408" : String

    fromString "81927398127398127389172893712893712893798123" |> Maybe.map toString -- Just "81927398127398127389172893712893712893798123" : Maybe String

-}
fromInt : Int -> Decimal
fromInt i =
    let
        ( s, e ) =
            sigExpFromInt ( i, 0 )

        integer =
            Integer.fromInt s
    in
    Decimal integer e
        |> moveZeroesToE minExponent


sigExpFromInt : ( Int, Int ) -> ( Int, Int )
sigExpFromInt ( i, e ) =
    case ( i // 10, remainderBy 10 i ) of
        ( 0, 0 ) ->
            ( i, e )

        ( _, 0 ) ->
            sigExpFromInt ( i // 10, e + 1 )

        _ ->
            ( i, e )


{-| Construct `Decimal` from `Integer`.

    fromInteger (Integer.fromString "812371263726178361298371987329810000" |> Maybe.withDefault Integer.zero) -- Decimal (Integer Positive [8732981,2983719,6178361,7126372,8123]) 4 : Decimal

    fromInteger Integer.zero -- Decimal Integer.Zero 0 : Decimal

    fromInteger (Integer.negate Integer.zillion) -- Decimal (Integer Negative [1]) 12 : Decimal

-}
fromInteger : Integer -> Decimal
fromInteger i =
    if Integer.eq i Integer.zero then
        Zero

    else
        let
            ( s, e ) =
                sigExpFromInteger i
        in
        Decimal s e
            |> moveZeroesToE minExponent


{-| Construct `Decimal` from `String`.

    fromString "0.0000000076128736" -- Just (Decimal (Integer Positive [6128736,7]) -16) : Maybe Decimal

    fromString "-7637370.761287360000" -- Just (Decimal (Integer Negative [6128736,6373707,7]) -8) : Maybe Decimal

    fromString "ajsdlkfj" -- Nothing : Maybe Decimal

-}



-- TODO: Explore using Parser library for toString


fromString : String -> Maybe Decimal
fromString =
    fromStringWithMinE minExponent


fromStringWithMinE : Exponent -> String -> Maybe Decimal
fromStringWithMinE minE s =
    case s of
        "0" ->
            Just Zero

        _ ->
            if Regex.contains decimalNotationRegex s then
                fromDecimalNotation minE s
                    |> Maybe.map (moveZeroesToE minE)

            else if Regex.contains scientificNotationRegex s then
                fromScientificNotation minE s
                    |> Maybe.map (moveZeroesToE minE)

            else
                Nothing


decimalNotationRegex : Regex
decimalNotationRegex =
    Maybe.withDefault Regex.never (Regex.fromString "^-?(\\d+\\.?\\d+|\\d+)$")


scientificNotationRegex : Regex
scientificNotationRegex =
    Maybe.withDefault Regex.never (Regex.fromString "^-?\\d{1}(\\.\\d*[1-9]|)e{1}-?\\d+$")


fromDecimalNotation : Exponent -> String -> Maybe Decimal
fromDecimalNotation minE s =
    case String.split "." s of
        i :: [] ->
            let
                trimmedSig =
                    trimTrailingZero i

                e =
                    String.length i - String.length trimmedSig
            in
            Maybe.map2 Decimal (Integer.fromString trimmedSig) (Just e)

        i :: f :: [] ->
            let
                trimmedF =
                    trimTrailingZero f

                -- TODO: Round dropped fractional digits instead of simply truncating
                f_ =
                    String.left (Basics.negate minE) trimmedF

                e =
                    Basics.negate (String.length f_)

                mbSig =
                    Integer.fromString (i ++ f_)
            in
            Maybe.map2 Decimal mbSig (Just e)

        _ ->
            Nothing


fromScientificNotation : Exponent -> String -> Maybe Decimal
fromScientificNotation minE s =
    case String.split "e" (String.toLower s) of
        co :: exp :: [] ->
            case String.split "." co of
                i :: [] ->
                    if String.length i > 1 then
                        Nothing

                    else
                        let
                            mbE =
                                String.toInt exp
                                    |> Maybe.andThen
                                        (\e ->
                                            if e < minE then
                                                Nothing

                                            else
                                                Just e
                                        )
                        in
                        if i == "0" then
                            Just Zero

                        else
                            Maybe.map2 Decimal (Integer.fromString i) mbE

                i :: f :: [] ->
                    let
                        mbE =
                            String.toInt exp
                                |> Maybe.map (\e -> e - String.length f)
                                |> Maybe.andThen
                                    (\e ->
                                        if e < minE then
                                            Nothing

                                        else
                                            Just e
                                    )
                    in
                    Maybe.map2 Decimal (Integer.fromString (i ++ f)) mbE

                _ ->
                    Nothing

        _ ->
            Nothing


{-| Construct `Decimal` from `Float`.

    fromFloat -8213.211 -- Decimal (Integer Negative [8213211]) -3 : Decimal

    fromFloat 0.0000000000000000000000000000000003323232238 -- Decimal (Integer Positive [3]) -34 : Decimal

    Entering numbers that cannot be represented by Javascript number type will result in incorrect representation. Use fromString in that case.

    fromFloat -1727816283761287361287631287.123123123123 |> toString -- "-1727816283761287400000000000" : String

    fromString "-1727816283761287361287631287.123123123123" |> Maybe.map toString -- Just "-1727816283761287361287631287.123123123123" : Maybe String

-}
fromFloat : Float -> Decimal
fromFloat f =
    Maybe.withDefault (Decimal Integer.zero 0) (fromString (String.fromFloat f))



-- String representation


{-| `Notation` type determines how to represent `Decimal` as `String`. `Dec` represents decimal notation such as `0.003`, `1231.6161`, `335`, or `33500`. `Sci` represents scientific notation such as `3e-3`, `1.2316161e3`, or `3.35e4`.
-}
type Notation
    = Dec
    | Sci


{-| Represent `Decimal` as `String`. Default notation is `Dec`.

    mul (fromFloat 8172387.1123) (fromFloat -3.3532) |> toString == "-27403648.46496436" : String

-}
toString : Decimal -> String
toString =
    toStringIn Dec


{-| Represent `Decimal` as `String` in desired notation.
-}
toStringIn : Notation -> Decimal -> String
toStringIn notation d =
    case d of
        Zero ->
            "0"

        Decimal s e ->
            case notation of
                Sci ->
                    let
                        ( co, exp ) =
                            normalize d

                        coString =
                            toString co

                        expString =
                            "e" ++ String.fromInt exp
                    in
                    coString ++ expString

                Dec ->
                    let
                        sign =
                            if Integer.gte s Integer.zero then
                                ""

                            else
                                "-"

                        sigString =
                            Integer.toString (Integer.abs s)

                        decString =
                            case Basics.compare e 0 of
                                GT ->
                                    sigString ++ String.repeat e "0"

                                EQ ->
                                    sigString

                                LT ->
                                    case Basics.compare (String.length sigString) (Basics.negate e) of
                                        GT ->
                                            String.join "." [ String.dropRight (Basics.negate e) sigString, String.right (Basics.negate e) sigString ]

                                        EQ ->
                                            "0." ++ sigString

                                        LT ->
                                            "0." ++ String.padLeft (Basics.negate e) '0' sigString
                    in
                    sign ++ decString



-- Deconstructor


{-| Get significand portion of `Decimal`.
-}
significand : Decimal -> Integer
significand d =
    case d of
        Zero ->
            Integer.zero

        Decimal s _ ->
            s


{-| Get exponent portion of `Decimal`.
-}
exponent : Decimal -> Exponent
exponent d =
    case d of
        Zero ->
            0

        Decimal _ e ->
            e



-- Basic operations


{-| Add two `Decimal`s.

    add (fromFloat 8172398.121) (fromFloat 0.0000001) |> toString -- "8172398.1210001" : String

    add (fromFloat 0.000032) (fromFloat -0.000032) |> toString -- "0" : String

    add (fromFloat -0.002) (fromFloat 0) |> toString -- "-0.002" : String

-}
add : Decimal -> Decimal -> Decimal
add d1 d2 =
    case toCommonE ( d1, d2 ) of
        ( Zero, _ ) ->
            d2

        ( _, Zero ) ->
            d1

        ( Decimal s1 e1, Decimal s2 e2 ) ->
            let
                resultS =
                    Integer.add s1 s2
            in
            Decimal resultS e1
                |> moveZeroesToE e1


{-| Subtract two `Decimal`s.

    sub (fromFloat 8172398.121) (fromFloat 0.0000001) |> toString -- "8172398.1209999" : String

    sub (fromFloat 0.000032) (fromFloat -0.000032) |> toString -- "0.000064" : String

    sub (fromFloat -0.002) (fromFloat 0) |> toString -- "-0.002" : String

-}
sub : Decimal -> Decimal -> Decimal
sub d1 d2 =
    add d1 (negate d2)


{-| Multiply two `Decimal`s.

    mul (fromFloat 8172398.121) (fromFloat 0.0000001) |> toString -- "0.8172398121" : String

    mul (fromFloat 0.000032) (fromFloat -0.000032) |> toString -- "-0.000000001024" : String

    mul (fromFloat -0.002) (fromFloat 0) |> toString -- "0" : String

-}
mul : Decimal -> Decimal -> Decimal
mul =
    mulToMinE minExponent


{-| Use this function when you want a multiplication with more fraction digits than what can be expressed with `minExponent`.
-}
mulToMinE : Int -> Decimal -> Decimal -> Decimal
mulToMinE minE d1 d2 =
    case ( d1, d2 ) of
        ( Zero, _ ) ->
            Zero

        ( _, Zero ) ->
            Zero

        ( Decimal s1 e1, Decimal s2 e2 ) ->
            let
                s_ =
                    Integer.mul s1 s2

                e_ =
                    e1 + e2
            in
            moveZeroesToE minE (Decimal s_ e_)


{-| Divide two `Decimal`s.

    div (fromFloat 8172398.121) (fromFloat 0.0000001) |> Maybe.map toString -- Just "81723981210000" : Maybe String

    div (fromFloat 0.000032) (fromFloat -0.000032) |> Maybe.map toString -- Just "-1" : Maybe String

    div (fromFloat 0) (fromFloat -0.002) |> Maybe.map toString -- Just "0" : Maybe String

    div (fromFloat -0.002) (fromFloat 0) |> Maybe.map toString -- Nothing : Maybe String

    Maybe.andThen (div (fromInt 1)) (fromString "100000000000000000000000000000000000000") |> Maybe.map toString -- Nothing : Maybe String

-}
div : Decimal -> Decimal -> Maybe Decimal
div =
    divToMinE minExponent


{-| Use this function when you want a division with more fraction digits than what can be expressed with `minExponent`.
-}
divToMinE : Int -> Decimal -> Decimal -> Maybe Decimal
divToMinE minE dividend divisor =
    case ( dividend, divisor ) of
        ( Zero, Zero ) ->
            Nothing

        ( Zero, _ ) ->
            Just Zero

        ( _, Zero ) ->
            Nothing

        ( Decimal dividendS dividendE, Decimal divisorS divisorE ) ->
            let
                initE =
                    dividendE - divisorE

                maybeResult =
                    divToMinE_ minE dividendS divisorS (Just ( Integer.zero, initE ))

                result =
                    Maybe.map (tupleToDecimal >> moveZeroesToE minE) maybeResult
            in
            result


tupleToDecimal : ( Significand, Exponent ) -> Decimal
tupleToDecimal ( s, e ) =
    Decimal s e


divToMinE_ : Int -> Significand -> Significand -> Maybe ( Significand, Exponent ) -> Maybe ( Significand, Exponent )
divToMinE_ minE dividend divisor =
    Maybe.andThen
        (\( qS, qE ) ->
            if Integer.eq dividend Integer.zero || qE < minE then
                Just ( qS, qE + 1 )

            else
                Maybe.andThen
                    (\( q, r ) ->
                        let
                            qS_ =
                                Integer.add (Integer.mul qS Integer.ten) q

                            dividend_ =
                                Integer.mul r Integer.ten
                        in
                        if Integer.eq r Integer.zero || Integer.countDigits q + qE < minE then
                            Just ( qS_, qE )

                        else
                            divToMinE_ minE dividend_ divisor (Just ( qS_, qE - 1 ))
                    )
                    (Integer.divmod dividend divisor)
        )



-- Sign modification functions


{-| Negate the sign of `Decimal`.

    negate (fromInt 1232139812) |> toString         == "-1232139812" : String

    negate (fromFloat -127.1232139812) |> toString  == "127.1232139812" : String

    negate (fromInt 0) |> toString                  == "0" : String

-}
negate : Decimal -> Decimal
negate d =
    case d of
        Zero ->
            Zero

        Decimal s e ->
            Decimal (Integer.negate s) e


{-| Get absolute value of `Decimal`.

    abs (fromInt 1232139812) |> toString        == "1232139812" : String

    abs (fromInt 0) |> toString                 == "0" : String

    abs (fromFloat -127.1232139812) |> toString == "127.1232139812" : String

-}
abs : Decimal -> Decimal
abs d =
    case d of
        Zero ->
            Zero

        Decimal s e ->
            Decimal (Integer.abs s) e



-- Comparison functions


{-| Compare two `Decimal`s.

    compare (fromFloat 23.223) (fromFloat 23.224) == LT : Order

    compare (fromFloat 23.223) (fromFloat 23.22) == GT : Order

    compare (fromFloat 23.223) (fromFloat -23.22) == GT : Order

    compare (fromFloat 23.223) (fromFloat 23.223) == EQ : Order

-}
compare : Decimal -> Decimal -> Order
compare d1 d2 =
    case toCommonE ( d1, d2 ) of
        ( Zero, Zero ) ->
            EQ

        ( Zero, Decimal s _ ) ->
            Integer.compare Integer.zero s

        ( Decimal s _, Zero ) ->
            Integer.compare s Integer.zero

        ( Decimal s1 e1, Decimal s2 e2 ) ->
            Integer.compare s1 s2


lt : Decimal -> Decimal -> Bool
lt d1 d2 =
    case compare d1 d2 of
        LT ->
            True

        _ ->
            False


gt : Decimal -> Decimal -> Bool
gt d1 d2 =
    case compare d1 d2 of
        GT ->
            True

        _ ->
            False


lte : Decimal -> Decimal -> Bool
lte d1 d2 =
    case compare d1 d2 of
        LT ->
            True

        EQ ->
            True

        _ ->
            False


gte : Decimal -> Decimal -> Bool
gte d1 d2 =
    case compare d1 d2 of
        GT ->
            True

        EQ ->
            True

        _ ->
            False


eq : Decimal -> Decimal -> Bool
eq d1 d2 =
    case compare d1 d2 of
        EQ ->
            True

        _ ->
            False



-- Rounding


{-| `RoundingContext` type can be used to specify rounding strategy. Specify `e` to designate the digit you would like to round to, and choose a `mode` for rounding. `e` of `0` will round to an integer; `e` of `-1` will round to the nearest tenths, `e` of `-2` to the nearest hundredths; `e` of `1` will round to tens, `e` of `2` to hundreds.
-}
type alias RoundingContext =
    { e : Exponent
    , mode : RoundingMode
    }


{-| `RoundingMode` type determines rounding strategy. Default strategy is `HalfToEven`.

Value | Down | Up | TowardsZero | AwayFromZero | HalfToEven
+1.8 | +1 | +2 | +1 | +2 | +2
+1.5 | +1 | +2 | +1 | +2 | +2
+1.2 | +1 | +2 | +1 | +2 | +1
+0.8 || 0 | +1 || 0 | +1 | +1
+0.5 || 0 | +1 || 0 | +1 || 0
+0.2 || 0 | +1 || 0 | +1 || 0
−0.2 | −1 || 0 || 0 | −1 || 0
−0.5 | −1 || 0 || 0 | −1 || 0
−0.8 | −1 || 0 || 0 | -1 | -1
−1.2 | −2 | −1 | −1 | −2 | −1
−1.5 | −2 | −1 | −1 | −2 | −2
−1.8 | −2 | −1 | −1 | −2 | −2

-}
type RoundingMode
    = Down
    | Up
    | TowardsZero
    | AwayFromZero
    | HalfToEven


{-| `round` to the nearest integer with `HalfToEven` mode.
-}
round : Decimal -> Decimal
round =
    roundWithContext { e = 0, mode = HalfToEven }


{-| `roundTo` given exponent with `HalfToEven` mode.
-}
roundTo : Exponent -> Decimal -> Decimal
roundTo e_ =
    roundWithContext { e = e_, mode = HalfToEven }


{-| `floor` to the nearest integer with `Down` mode.
-}
floor : Decimal -> Decimal
floor =
    roundWithContext { e = 0, mode = Down }


{-| `ceiling` to the nearest integer with `Up` mode.
-}
ceiling : Decimal -> Decimal
ceiling =
    roundWithContext { e = 0, mode = Up }


{-| `truncate` to the nearest integer with `TowardsZero` mode.
-}
truncate : Decimal -> Decimal
truncate =
    roundWithContext { e = 0, mode = TowardsZero }


{-| Specify a `RoundingContext` to customize the rounding strategy.
-}
roundWithContext : RoundingContext -> Decimal -> Decimal
roundWithContext context d =
    case d of
        Zero ->
            Zero

        Decimal s e ->
            if context.e <= e then
                d

            else if context.e > Integer.countDigits s + e then
                Zero

            else
                let
                    eDiff =
                        context.e - e

                    divisor =
                        if eDiff < 11 then
                            Integer.fromInt (10 ^ eDiff)

                        else
                            Maybe.withDefault Integer.zero (Integer.fromString ("1" ++ String.repeat eDiff "0"))

                    s_ =
                        divRound context.mode s divisor
                in
                if Integer.eq s_ Integer.zero then
                    Zero

                else
                    moveZeroesToE (Basics.min context.e minExponent) (Decimal s_ context.e)


divRound : RoundingMode -> Integer -> Integer -> Integer
divRound mode i1 i2 =
    case mode of
        Down ->
            roundDown i1 i2

        Up ->
            roundUp i1 i2

        TowardsZero ->
            roundTowardsZero i1 i2

        AwayFromZero ->
            roundAwayFromZero i1 i2

        HalfToEven ->
            roundHalfToEven i1 i2



-- TODO: Remove usage of Maybe.withDefault


roundHalfToEven : Integer -> Integer -> Integer
roundHalfToEven i1 i2 =
    let
        ( q, r ) =
            Integer.divmod i1 i2
                |> Maybe.withDefault ( Integer.zero, Integer.zero )

        mod =
            case Integer.compare (Integer.abs (Integer.mul (Integer.fromInt 2) r)) (Integer.abs i2) of
                LT ->
                    Integer.zero

                EQ ->
                    if (Integer.remainderBy q (Integer.fromInt 2) |> Maybe.withDefault Integer.zero) /= Integer.zero then
                        case Integer.compare i1 Integer.zero of
                            LT ->
                                Integer.fromInt -1

                            EQ ->
                                Integer.zero

                            GT ->
                                Integer.one

                    else
                        Integer.zero

                GT ->
                    case Integer.compare i1 Integer.zero of
                        LT ->
                            Integer.fromInt -1

                        EQ ->
                            Integer.zero

                        GT ->
                            Integer.one
    in
    Integer.add q mod


roundTowardsZero : Integer -> Integer -> Integer
roundTowardsZero i1 i2 =
    Integer.divmod i1 i2
        |> Maybe.withDefault ( Integer.zero, Integer.zero )
        |> Tuple.first


roundDown : Integer -> Integer -> Integer
roundDown i1 i2 =
    let
        ( q, r ) =
            Integer.divmod i1 i2
                |> Maybe.withDefault ( Integer.zero, Integer.zero )

        z =
            Integer.zero

        isPositive =
            Integer.lt i1 z == Integer.lt i2 z
    in
    if Integer.eq r z then
        q

    else
        case Integer.compare q z of
            LT ->
                Integer.add q (Integer.fromInt -1)

            EQ ->
                if isPositive then
                    q

                else
                    Integer.add q (Integer.fromInt -1)

            GT ->
                q


roundUp : Integer -> Integer -> Integer
roundUp i1 i2 =
    let
        ( q, r ) =
            Integer.divmod i1 i2
                |> Maybe.withDefault ( Integer.zero, Integer.zero )

        z =
            Integer.zero

        isPositive =
            Integer.lt i1 z == Integer.lt i2 z
    in
    if Integer.eq r z then
        q

    else
        case Integer.compare q z of
            LT ->
                q

            EQ ->
                if isPositive then
                    Integer.add q Integer.one

                else
                    q

            GT ->
                Integer.add q Integer.one


roundAwayFromZero : Integer -> Integer -> Integer
roundAwayFromZero i1 i2 =
    let
        ( q, r ) =
            Integer.divmod i1 i2
                |> Maybe.withDefault ( Integer.zero, Integer.zero )

        z =
            Integer.zero

        isPositive =
            Integer.lt i1 z == Integer.lt i2 z
    in
    if Integer.eq r z then
        q

    else
        case Integer.compare q z of
            LT ->
                Integer.add q (Integer.fromInt -1)

            EQ ->
                if isPositive then
                    Integer.add q Integer.one

                else
                    Integer.add q (Integer.fromInt -1)

            GT ->
                Integer.add q Integer.one



-- Square root


{-| Calculate Square root using Babylonian method.
-}
sqrt : Decimal -> Maybe Decimal
sqrt =
    sqrtToMinE minExponent


{-| Use this function when you want a square root with more fraction digits than what can be expressed with `minExponent`.
-}
sqrtToMinE : Exponent -> Decimal -> Maybe Decimal
sqrtToMinE minE d =
    case d of
        Zero ->
            Just Zero

        Decimal s e ->
            if Integer.lt s Integer.zero then
                Nothing

            else
                Just (moveZeroesToE minE (sqrt_ minE d (guessSqrt d)))



-- TODO: Remove usage of Maybe.withDefault


sqrt_ : Exponent -> Decimal -> Decimal -> Decimal
sqrt_ minE d guess =
    if lte (abs (sub d (mulToMinE minE guess guess))) (Decimal (Integer.fromInt 1) (minE + 1)) then
        guess

    else
        let
            newGuess =
                divToMinE minE d guess
                    |> Maybe.map (add guess)
                    |> Maybe.andThen (\dividend -> divToMinE minE dividend (fromInt 2))
                    |> Maybe.withDefault (fromInt 0)
        in
        if eq guess newGuess then
            guess

        else
            sqrt_ minE d newGuess


guessSqrt : Decimal -> Decimal
guessSqrt d =
    let
        ( normalizedD, e ) =
            normalize d
    in
    if remainderBy 2 e == 0 then
        Decimal (Integer.fromInt 2) (e // 2)

    else
        Decimal (Integer.fromInt 6) ((e - 1) // 2)



-- Shared helper functions


moveZeroesToE : Int -> Decimal -> Decimal
moveZeroesToE minE d =
    case d of
        Zero ->
            Zero

        Decimal s e ->
            let
                ( s_, e_ ) =
                    sigExpFromInteger s

                e__ =
                    Basics.max (e + e_) minE

                -- TODO: Round dropped fractional digits instead of simply truncating
                -- TODO: Remove usage of Maybe.withDefault
                s__ =
                    if e + e_ < minE then
                        s_ |> Integer.toString >> String.dropRight (minE - (e + e_)) >> Integer.fromString >> Maybe.withDefault Integer.zero

                    else
                        s_
            in
            if s__ == Integer.fromInt 0 then
                Zero

            else if s__ |> Integer.toString >> String.endsWith "0" then
                moveZeroesToE minE (Decimal s__ e__)

            else
                Decimal s__ e__


trimTrailingZero : String -> String
trimTrailingZero =
    Regex.replace (Maybe.withDefault Regex.never (Regex.fromString "0*$")) (\_ -> "")


sigExpFromInteger : Integer -> ( Integer, Int )
sigExpFromInteger i =
    let
        s =
            Integer.toString i

        s_ =
            trimTrailingZero (Integer.toString i)

        i_ =
            Maybe.withDefault Integer.zero (Integer.fromString s_)

        e =
            String.length s - String.length s_
    in
    ( i_, e )


toCommonE : ( Decimal, Decimal ) -> ( Decimal, Decimal )
toCommonE ( d1, d2 ) =
    case ( d1, d2 ) of
        ( Zero, Zero ) ->
            ( Zero, Zero )

        ( Zero, _ ) ->
            ( Zero, d2 )

        ( _, Zero ) ->
            ( d1, Zero )

        ( Decimal s1 e1, Decimal s2 e2 ) ->
            if e1 == e2 then
                ( d1, d2 )

            else
                let
                    commonE =
                        Basics.min e1 e2

                    eDiff =
                        Basics.max e1 e2 - Basics.min e1 e2

                    eEqualizer =
                        if eDiff < 11 then
                            Integer.fromInt (10 ^ eDiff)

                        else
                            Maybe.withDefault Integer.zero (Integer.fromString ("1" ++ String.repeat eDiff "0"))
                in
                if e1 > e2 then
                    ( Decimal (Integer.mul s1 eEqualizer) commonE, Decimal s2 commonE )

                else
                    ( Decimal s1 commonE, Decimal (Integer.mul s2 eEqualizer) commonE )


normalize : Decimal -> ( Decimal, Int )
normalize d =
    case d of
        Zero ->
            ( Zero, 0 )

        Decimal s e ->
            let
                coefficientE =
                    Basics.negate (Integer.countDigits s - 1)
            in
            ( Decimal s coefficientE, e - coefficientE )

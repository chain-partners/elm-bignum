module Decimal
    exposing
        ( Decimal
        , fromInt
        , fromInteger
        , fromFloat
        , fromString
        , toString
        , add
        , sub
        , mul
        , div
        , unsafeDiv
        , negate
        , abs
        , compare
        , lt
        , lte
        , gt
        , gte
        , eq
        , round
        , roundTo
        , ceiling
        , floor
        , truncate
        , roundWithContext
        , RoundingMethod(..)
        , RoundingContext
        )

import Char
import Integer as Integer exposing (Integer)
import Regex


-- Types


type alias Significand =
    Integer


type alias Exponent =
    Int


type Decimal
    = Decimal Significand Exponent
    | Zero



-- Constants


maxPrecision : Int
maxPrecision =
    -20



-- Constructors


fromInt : Int -> Decimal
fromInt i =
    let
        ( s, e ) =
            getSigExpFromInt ( Basics.abs i, 0 )

        integer =
            if i >= 0 then
                Integer.fromInt s
            else
                Integer.negate (Integer.fromInt s)
    in
        if integer == Integer.zero then
            Zero
        else
            Decimal integer e


getSigExpFromInt : ( Int, Int ) -> ( Int, Int )
getSigExpFromInt ( i, e ) =
    case ( i // 10, rem i 10 ) of
        ( 0, 0 ) ->
            ( i, e )

        ( _, 0 ) ->
            getSigExpFromInt ( i // 10, e + 1 )

        _ ->
            ( i, e )


fromInteger : Integer -> Decimal
fromInteger i =
    if Integer.eq i Integer.zero then
        Zero
    else
        let
            s =
                (Integer.toString i)

            s_ =
                trimTrailingZero (Integer.toString i)

            e =
                String.length s - String.length s_
        in
            Decimal (Maybe.withDefault Integer.zero (Integer.fromString s_)) e


fromString : String -> Maybe Decimal
fromString s =
    case s of
        "0" ->
            Just Zero

        _ ->
            if isValidString s then
                fromString_ s
            else
                Nothing


isValidString : String -> Bool
isValidString s =
    let
        validateSign : String -> Bool
        validateSign s =
            Regex.contains (Regex.regex "[0-9]|-") (String.left 1 s)

        validateSeparator : String -> Bool
        validateSeparator s =
            (s |> String.filter ((==) '.') |> String.length) <= 1

        validateDigits : String -> Bool
        validateDigits s =
            String.all (\c -> c == '.' || Char.isDigit c || c == '-') s
    in
        validateSign s && validateDigits s && validateSeparator s


fromString_ : String -> Maybe Decimal
fromString_ s =
    let
        parts =
            String.split "." s
    in
        case parts of
            i :: [] ->
                let
                    s_ =
                        trimTrailingZero i

                    e =
                        String.length i - String.length s_
                in
                    Maybe.map2 Decimal (Integer.fromString s_) (Just e)

            i :: f :: [] ->
                let
                    f_ =
                        trimTrailingZero f

                    e =
                        Basics.negate (String.length f_)

                    integer =
                        Integer.fromString (i ++ f_)
                in
                    if integer == Just Integer.zero then
                        Just Zero
                    else
                        Maybe.map (\i -> Decimal i e) integer

            _ ->
                Nothing


trimTrailingZero : String -> String
trimTrailingZero =
    Regex.replace Regex.All (Regex.regex "(?<=[1-9])0*$") (\_ -> "")


fromFloat : Float -> Decimal
fromFloat f =
    Maybe.withDefault Zero (fromString_ (Basics.toString f))


toString : Decimal -> String
toString d =
    case d of
        Zero ->
            "0"

        Decimal i e ->
            if Integer.gte i Integer.zero then
                applyExponent e (Integer.toString i)
            else
                "-" ++ (applyExponent e (Integer.toString (Integer.abs i)))


applyExponent : Int -> String -> String
applyExponent e s =
    case Basics.compare e 0 of
        GT ->
            s ++ (String.repeat e "0")

        EQ ->
            s

        LT ->
            let
                e_ =
                    Basics.abs e

                l =
                    String.length s
            in
                case Basics.compare l e_ of
                    GT ->
                        String.slice 0 e s ++ "." ++ String.slice e l s

                    EQ ->
                        "0." ++ s

                    LT ->
                        "0." ++ String.padLeft e_ '0' s



-- Basic operations


add : Decimal -> Decimal -> Decimal
add d1 d2 =
    case ( d1, d2 ) of
        ( Zero, _ ) ->
            d2

        ( _, Zero ) ->
            d1

        ( Decimal s1 e1, Decimal s2 e2 ) ->
            let
                ( s, e ) =
                    if e1 >= e2 then
                        let
                            normalizer =
                                if (e1 - e2) < 11 then
                                    Integer.fromInt (10 ^ (e1 - e2))
                                else
                                    Maybe.withDefault Integer.zero (Integer.fromString ("1" ++ (String.repeat (e1 - e2) "0")))
                        in
                            ( Integer.add (Integer.mul s1 normalizer) s2, e2 )
                    else
                        let
                            normalizer =
                                if (e2 - e1) < 11 then
                                    Integer.fromInt (10 ^ (e2 - e1))
                                else
                                    Maybe.withDefault Integer.zero (Integer.fromString ("1" ++ (String.repeat (e2 - e1) "0")))
                        in
                            ( Integer.add s1 (Integer.mul s2 normalizer), e1 )
            in
                if s == Integer.fromInt 0 then
                    Zero
                else
                    renormalizeDecimal (Decimal s e)


sub : Decimal -> Decimal -> Decimal
sub d1 d2 =
    add d1 (negate d2)


mul : Decimal -> Decimal -> Decimal
mul d1 d2 =
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

                d_ =
                    Decimal s_ e_
            in
                renormalizeDecimal d_



-- In case of infinite decimal, stop calculation at 10 ^ -20


div : Decimal -> Decimal -> Maybe Decimal
div d1 d2 =
    case ( d1, d2 ) of
        ( Zero, _ ) ->
            Just Zero

        ( _, Zero ) ->
            Nothing

        ( Decimal s1 e1, Decimal s2 e2 ) ->
            let
                startingE =
                    e1 - e2

                zeroInt =
                    Integer.zero

                isPositive =
                    Integer.lt s1 zeroInt == Integer.lt s2 zeroInt

                ( s, e ) =
                    div_ (Integer.abs s1) (Integer.abs s2) ( zeroInt, startingE )

                s_ =
                    if isPositive then
                        s
                    else
                        Integer.negate s
            in
                Just (renormalizeDecimal (Decimal s_ e))


div_ : Significand -> Significand -> ( Significand, Exponent ) -> ( Significand, Exponent )
div_ s1 s2 ( s, e ) =
    if e < maxPrecision || s1 == Integer.zero then
        ( s, e + 1 )
    else
        let
            digit =
                Integer.ten

            ( q, r ) =
                Integer.unsafeDivmod s1 s2

            s_ =
                Integer.add (Integer.mul s digit) q

            s1_ =
                Integer.mul r digit
        in
            div_ s1_ s2 ( s_, e - 1 )


unsafeDiv : Decimal -> Decimal -> Decimal
unsafeDiv d1 d2 =
    div d1 d2
        |> Maybe.withDefault Zero



-- Sign modification functions


negate : Decimal -> Decimal
negate d =
    case d of
        Zero ->
            Zero

        Decimal s e ->
            Decimal (Integer.negate s) e


abs : Decimal -> Decimal
abs d =
    case d of
        Zero ->
            Zero

        Decimal s e ->
            Decimal (Integer.abs s) e



-- Comparison functions


compare : Decimal -> Decimal -> Order
compare d1 d2 =
    case ( d1, d2 ) of
        ( Zero, Zero ) ->
            EQ

        ( Decimal s e, Zero ) ->
            Integer.compare s Integer.zero

        ( Zero, Decimal s e ) ->
            Integer.compare Integer.zero s

        ( Decimal s1 e1, Decimal s2 e2 ) ->
            case Basics.compare e1 e2 of
                GT ->
                    let
                        normalizer =
                            if (e1 - e2) < 11 then
                                Integer.fromInt (10 ^ (e1 - e2))
                            else
                                Maybe.withDefault Integer.zero (Integer.fromString ("1" ++ (String.repeat (e1 - e2) "0")))
                    in
                        Integer.compare (Integer.mul s1 normalizer) s2

                EQ ->
                    Integer.compare s1 s2

                LT ->
                    let
                        normalizer =
                            if (e2 - e1) < 11 then
                                Integer.fromInt (10 ^ (e2 - e1))
                            else
                                Maybe.withDefault Integer.zero (Integer.fromString ("1" ++ (String.repeat (e2 - e1) "0")))
                    in
                        Integer.compare s1 (Integer.mul s2 normalizer)


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


type alias RoundingContext =
    { e : Exponent
    , method : RoundingMethod
    }


type RoundingMethod
    = Down
    | Up
    | TowardsZero
    | AwayFromZero
    | HalfToEven


round : Decimal -> Decimal
round =
    roundWithContext { e = 0, method = HalfToEven }


roundTo : Exponent -> Decimal -> Decimal
roundTo e =
    roundWithContext { e = e, method = HalfToEven }


floor : Decimal -> Decimal
floor =
    roundWithContext { e = 0, method = Down }


ceiling : Decimal -> Decimal
ceiling =
    roundWithContext { e = 0, method = Up }


truncate : Decimal -> Decimal
truncate =
    roundWithContext { e = 0, method = TowardsZero }


roundWithContext : RoundingContext -> Decimal -> Decimal
roundWithContext { e, method } d =
    case d of
        Zero ->
            Zero

        Decimal ds de ->
            if e <= de then
                d
            else if e > (Integer.countDigits ds) + de + 1 then
                Zero
            else
                let
                    divisor =
                        Integer.fromInt (10 ^ (e - de))

                    ds_ =
                        divRound method ds divisor
                in
                    if Integer.eq ds_ Integer.zero then
                        Zero
                    else
                        renormalizeDecimal (Decimal ds_ e)


divRound : RoundingMethod -> Integer -> Integer -> Integer
divRound method i1 i2 =
    case method of
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


roundHalfToEven : Integer -> Integer -> Integer
roundHalfToEven i1 i2 =
    let
        ( q, r ) =
            Integer.unsafeDivmod i1 i2

        mod =
            case Integer.compare (Integer.abs (Integer.mul (Integer.fromInt 2) r)) (Integer.abs i2) of
                LT ->
                    Integer.zero

                EQ ->
                    if Integer.unsafeRem q (Integer.fromInt 2) /= Integer.zero then
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
    Integer.unsafeDivmod i1 i2
        |> Tuple.first


roundDown : Integer -> Integer -> Integer
roundDown i1 i2 =
    let
        ( q, r ) =
            Integer.unsafeDivmod i1 i2

        z =
            Integer.zero

        isPositive =
            (Integer.lt i1 z) == (Integer.lt i2 z)
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
            Integer.unsafeDivmod i1 i2

        z =
            Integer.fromInt 0

        isPositive =
            (Integer.lt i1 z) == (Integer.lt i2 z)
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
            Integer.unsafeDivmod i1 i2

        z =
            Integer.zero

        isPositive =
            (Integer.lt i1 z) == (Integer.lt i2 z)
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



-- Shared helper functions


renormalizeDecimal : Decimal -> Decimal
renormalizeDecimal d =
    case d of
        Zero ->
            Zero

        Decimal s e ->
            let
                ( s_, e_ ) =
                    getSigExpFromInteger s
            in
                Decimal s_ (e + e_)


getSigExpFromInteger : Integer -> ( Integer, Int )
getSigExpFromInteger i =
    let
        s =
            (Integer.toString i)

        s_ =
            trimTrailingZero (Integer.toString i)

        i_ =
            Maybe.withDefault Integer.zero (Integer.fromString s_)

        e =
            String.length s - String.length s_
    in
        ( i_, e )

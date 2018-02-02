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
        , safeDiv
        , negate
        , trimTrailingZero
        , abs
        , compare
        , lt
        , lte
        , gt
        , gte
        , eq
        )

import Integer exposing (Integer)
import Char
import Regex


-- Types


type alias Significand =
    Integer


type alias Exponent =
    Int


type Decimal
    = Decimal Significand Exponent
    | Zero



-- Constructors


fromInt : Int -> Decimal
fromInt i =
    let
        ( s, e ) =
            getSigExpFromInt ( i, 0 )

        integer =
            Integer.fromInt s
    in
        if integer == (Integer.fromInt 0) then
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
    let
        z =
            Integer.fromInt 0
    in
        if Integer.eq i z then
            Zero
        else
            let
                ( i_, e ) =
                    getSigExpFromInteger ( i, 0 )
            in
                Decimal i_ e


getSigExpFromInteger : ( Integer, Int ) -> ( Integer, Int )
getSigExpFromInteger ( i, e ) =
    let
        ten =
            Integer.fromInt 10

        ( q, r ) =
            Integer.divmod i ten

        z =
            Integer.fromInt 0
    in
        case ( q == z, r == z ) of
            ( True, True ) ->
                ( i, e )

            ( _, True ) ->
                getSigExpFromInteger ( q, e + 1 )

            _ ->
                ( i, e )


fromString : String -> Maybe Decimal
fromString s =
    case s of
        "0" ->
            Just Zero

        _ ->
            s
                |> validateString
                |> Maybe.andThen parseString


validateString : String -> Maybe String
validateString s =
    let
        validateSign : String -> Bool
        validateSign s =
            Regex.contains (Regex.regex "[0-9]|-") (String.left 1 s)

        validateSeparator : String -> Bool
        validateSeparator s =
            (s |> String.filter isSeparator |> String.length) <= 1

        validateDigits : String -> Bool
        validateDigits s =
            String.all (\c -> isSeparator c || Char.isDigit c || c == '-') s
    in
        if validateSign s && validateDigits s && validateSeparator s then
            Just s
        else
            Nothing


isSeparator : Char -> Bool
isSeparator c =
    c == '.' || c == ','


parseString : String -> Maybe Decimal
parseString s =
    let
        sepIndex =
            (String.indices "." s)
                ++ (String.indices "," s)
                |> List.head
    in
        case sepIndex of
            Nothing ->
                let
                    ( sig, exp ) =
                        getSigExpFromString s
                in
                    Maybe.map2 Decimal (Integer.fromString sig) (Just exp)

            Just index ->
                let
                    s_ =
                        trimTrailingZero s

                    e =
                        s_
                            |> String.dropLeft (index + 1)
                            |> String.length
                            |> Basics.negate

                    i =
                        s_
                            |> String.filter (isSeparator >> not)
                            |> Integer.fromString
                in
                    if i == Just (Integer.fromInt 0) then
                        Just Zero
                    else
                        Maybe.map ((flip Decimal) e) i


getSigExpFromString : String -> ( String, Int )
getSigExpFromString =
    String.foldr
        (\c ( acc, e ) ->
            if c == '0' && acc == "" then
                ( acc, e + 1 )
            else
                ( String.cons c acc, e )
        )
        ( "", 0 )


trimTrailingZero : String -> String
trimTrailingZero =
    String.foldr
        (\c cs ->
            if c == '0' && cs == "" then
                ""
            else
                String.cons c cs
        )
        ""


fromFloat : Float -> Decimal
fromFloat f =
    f
        |> Basics.toString
        |> fromString
        |> Maybe.withDefault Zero


toString : Decimal -> String
toString d =
    case d of
        Zero ->
            "0"

        Decimal i e ->
            if Integer.gte i (Integer.fromInt 0) then
                applyExponent (Integer.toString i) e
            else
                "-" ++ (applyExponent (Integer.toString (Integer.abs i)) e)


applyExponent : String -> Int -> String
applyExponent s e =
    case Basics.compare e 0 of
        GT ->
            s ++ (String.repeat e "0")

        EQ ->
            s

        LT ->
            let
                e_ =
                    Basics.abs e

                ( i, f ) =
                    case Basics.compare (String.length s) e_ of
                        GT ->
                            ( String.dropRight e_ s, String.right e_ s )

                        EQ ->
                            ( "0", s )

                        LT ->
                            ( "0", String.padLeft e_ '0' s )
            in
                i ++ "." ++ f



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
                                "1"
                                    ++ (String.repeat (e1 - e2) "0")
                                    |> Integer.fromString
                                    |> Maybe.withDefault (Integer.fromInt 0)
                        in
                            ( Integer.add (Integer.mul s1 normalizer) s2, e2 )
                    else
                        let
                            normalizer =
                                "1"
                                    ++ (String.repeat (e2 - e1) "0")
                                    |> Integer.fromString
                                    |> Maybe.withDefault (Integer.fromInt 0)
                        in
                            ( Integer.add s1 (Integer.mul s2 normalizer), e1 )
            in
                if s == Integer.fromInt 0 then
                    Zero
                else
                    renormalizeDecimal (Decimal s e)


renormalizeDecimal : Decimal -> Decimal
renormalizeDecimal d =
    case d of
        Zero ->
            Zero

        Decimal s e ->
            let
                ( s_, e_ ) =
                    getSigExpFromInteger ( s, e )
            in
                Decimal s_ e_


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


safeDiv : Decimal -> Decimal -> Maybe Decimal
safeDiv d1 d2 =
    case ( d1, d2 ) of
        ( Zero, _ ) ->
            Just Zero

        ( _, Zero ) ->
            Nothing

        ( Decimal s1 e1, Decimal s2 e2 ) ->
            let
                startingExp =
                    e1 - e2

                zeroInt =
                    Integer.fromInt 0

                isPositive =
                    Integer.lt s1 zeroInt == Integer.lt s2 zeroInt

                ( s, e ) =
                    safeDiv_ (Integer.abs s1) (Integer.abs s2) ( zeroInt, startingExp )

                s_ =
                    if isPositive then
                        s
                    else
                        Integer.negate s
            in
                Just (renormalizeDecimal (Decimal s_ e))


safeDiv_ : Significand -> Significand -> ( Significand, Exponent ) -> ( Significand, Exponent )
safeDiv_ s1 s2 ( s, e ) =
    let
        hasNoRemainder : Significand -> Bool
        hasNoRemainder s =
            s == Integer.fromInt 0

        hasGivenPrecision : Exponent -> Exponent -> Bool
        hasGivenPrecision targetE currentE =
            currentE < targetE
    in
        if hasGivenPrecision -20 e || hasNoRemainder s1 then
            ( s, e + 1 )
        else
            let
                digit =
                    Integer.fromInt 10

                ( q, r ) =
                    Integer.divmod s1 s2

                s_ =
                    Integer.add (Integer.mul s digit) q

                e_ =
                    e - 1

                s1_ =
                    Integer.mul r digit
            in
                safeDiv_ s1_ s2 ( s_, e_ )



-- Sign and other stuff


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


compare : Decimal -> Decimal -> Order
compare d1 d2 =
    case ( d1, d2 ) of
        ( Zero, Zero ) ->
            EQ

        ( Decimal s e, Zero ) ->
            Integer.compare s (Integer.fromInt 0)

        ( Zero, Decimal s e ) ->
            Integer.compare (Integer.fromInt 0) s

        ( Decimal s1 e1, Decimal s2 e2 ) ->
            case Basics.compare e1 e2 of
                GT ->
                    Integer.compare (Integer.mul s1 (Integer.fromInt (10 ^ (e1 - e2)))) s2

                EQ ->
                    Integer.compare s1 s2

                LT ->
                    Integer.compare s1 (Integer.mul s2 (Integer.fromInt (10 ^ (e2 - e1))))


lt : Decimal -> Decimal -> Bool
lt i1 i2 =
    case compare i1 i2 of
        LT ->
            True

        _ ->
            False


gt : Decimal -> Decimal -> Bool
gt i1 i2 =
    case compare i1 i2 of
        GT ->
            True

        _ ->
            False


lte : Decimal -> Decimal -> Bool
lte i1 i2 =
    case compare i1 i2 of
        LT ->
            True

        EQ ->
            True

        _ ->
            False


gte : Decimal -> Decimal -> Bool
gte i1 i2 =
    case compare i1 i2 of
        GT ->
            True

        EQ ->
            True

        _ ->
            False


eq : Decimal -> Decimal -> Bool
eq i1 i2 =
    case compare i1 i2 of
        EQ ->
            True

        _ ->
            False

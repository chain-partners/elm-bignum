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
        , negate
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
        insigFigs =
            countInsignificantFiguresFromInt i 0

        i_ =
            i // (10 ^ insigFigs)

        significand =
            Integer.fromInt i_

        e =
            insigFigs
    in
        Decimal significand e


countInsignificantFiguresFromInt : Int -> Int -> Int
countInsignificantFiguresFromInt i acc =
    case rem i 10 of
        0 ->
            countInsignificantFiguresFromInt (i // 10) (acc + 1)

        _ ->
            acc


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
                insigFigs =
                    countInsignificantFiguresFromInteger i 0

                i_ =
                    Integer.div i (Integer.fromInt (10 ^ insigFigs))
            in
                Decimal i_ insigFigs


countInsignificantFiguresFromInteger : Integer -> Int -> Int
countInsignificantFiguresFromInteger i acc =
    let
        ten =
            Integer.fromInt 10

        i_ =
            Integer.rem i ten

        z =
            Integer.fromInt 0
    in
        if Integer.eq i_ z then
            countInsignificantFiguresFromInteger (Integer.div i ten) (acc + 1)
        else
            acc


fromString : String -> Maybe Decimal
fromString s =
    s
        |> validateString
        |> Maybe.andThen parseString


validateString : String -> Maybe String
validateString s =
    let
        validateSign : String -> Bool
        validateSign s =
            Regex.contains (Regex.regex "[0-9]|-") (String.left 1 s)

        isSeparator : Char -> Bool
        isSeparator c =
            c == '.' || c == ','

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


parseString : String -> Maybe Decimal
parseString s =
    let
        separator =
            String.filter (\c -> c == '.' || c == ',') s
    in
        if String.isEmpty separator then
            Maybe.map2 Decimal (Integer.fromString s) (Just 0)
        else
            let
                iAndF =
                    String.split separator s

                i =
                    List.head iAndF

                f =
                    iAndF |> List.tail >> Maybe.andThen List.head

                i_ =
                    Maybe.map2 (++) i f
                        |> Maybe.andThen Integer.fromString

                e =
                    Maybe.map (String.length >> Basics.negate) f
            in
                Maybe.map2 Decimal i_ e


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
            applyExponent (Integer.toString i) e


applyExponent : String -> Int -> String
applyExponent s e =
    case compare e 0 of
        GT ->
            s ++ (String.repeat e "0")

        EQ ->
            s

        LT ->
            let
                i =
                    String.dropRight (Basics.abs e) s

                i_ =
                    if String.isEmpty i then
                        "0"
                    else
                        i

                f =
                    String.right (Basics.abs e) s
            in
                i_ ++ "." ++ f



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
                expDiff =
                    abs (e1 - e2)

                s =
                    if e1 >= e2 then
                        Integer.add s1 (Integer.mul s2 (Integer.fromInt (10 ^ expDiff)))
                    else
                        Integer.add s2 (Integer.mul s1 (Integer.fromInt (10 ^ expDiff)))

                d_ =
                    Decimal s (min e1 e2)
            in
                renormalizeDecimal d_


renormalizeDecimal : Decimal -> Decimal
renormalizeDecimal d =
    case d of
        Zero ->
            Zero

        Decimal s e ->
            let
                insigFigs =
                    countInsignificantFiguresFromInteger s 0

                s_ =
                    Integer.div s (Integer.fromInt (10 ^ insigFigs))

                e_ =
                    e + insigFigs
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



-- TODO : Impelement division
-- Sign and other stuff


negate : Decimal -> Decimal
negate d =
    case d of
        Zero ->
            Zero

        Decimal s e ->
            Decimal (Integer.negate s) e

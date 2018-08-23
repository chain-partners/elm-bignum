module Integer
    exposing
        ( Integer
        , fromInt
        , fromString
        , zero
        , one
        , ten
        , hundred
        , thousand
        , million
        , billion
        , zillion
        , add
        , sub
        , mul
        , divmod
        , unsafeDivmod
        , div
        , unsafeDiv
        , rem
        , unsafeRem
        , abs
        , negate
        , compare
        , lt
        , lte
        , gt
        , gte
        , eq
        , toString
        )

import Char
import Regex


-- Types


type Integer
    = Integer Sign Magnitude
    | Zero


type Sign
    = Positive
    | Negative


type alias Digit =
    Int


type alias Base =
    Int



-- Magnitude is in Little Endian


type alias Magnitude =
    List Digit



-- Constants


defaultBase : Base
defaultBase =
    10 ^ 7



-- Constructors


fromInt : Int -> Integer
fromInt i =
    case Basics.compare i 0 of
        GT ->
            Integer Positive (magnitudeFromInt i)

        EQ ->
            Zero

        LT ->
            Integer Negative (magnitudeFromInt (Basics.abs i))


magnitudeFromInt : Int -> Magnitude
magnitudeFromInt =
    magnitudeFromInt_ []


magnitudeFromInt_ : Magnitude -> Int -> Magnitude
magnitudeFromInt_ acc i =
    let
        q =
            i // defaultBase
    in
        if q == 0 then
            List.reverse (i :: acc)
        else
            magnitudeFromInt_ ((Basics.rem i defaultBase) :: acc) q


fromString : String -> Maybe Integer
fromString s =
    if isValidString s then
        let
            ( sign, num ) =
                if String.startsWith "-" s then
                    ( Negative, trimLeadingZeroFromStr (String.dropLeft 1 s) )
                else
                    ( Positive, trimLeadingZeroFromStr s )
        in
            if String.isEmpty num then
                Just Zero
            else
                Maybe.map (Integer sign) (magnitudeFromString num)
    else
        Nothing


isValidString : String -> Bool
isValidString =
    Regex.contains (Regex.regex "^-?\\d+$")


magnitudeFromString : String -> Maybe Magnitude
magnitudeFromString s =
    combine (List.map (String.toInt >> Result.toMaybe) (splitFromEndBy 7 [] s))


splitFromEndBy : Int -> List String -> String -> List String
splitFromEndBy n acc s =
    if s == "" then
        acc
    else
        let
            chunk =
                String.right n s

            rest =
                String.dropRight n s
        in
            splitFromEndBy n (chunk :: acc) rest


combine : List (Maybe a) -> Maybe (List a)
combine =
    List.foldl
        (\x acc ->
            case x of
                Nothing ->
                    Nothing

                Just i ->
                    Maybe.map ((::) i) acc
        )
        (Just [])


zero : Integer
zero =
    Zero


one : Integer
one =
    Integer Positive [ 1 ]


ten : Integer
ten =
    Integer Positive [ 10 ]


hundred : Integer
hundred =
    Integer Positive [ 100 ]


thousand : Integer
thousand =
    Integer Positive [ 1000 ]


million : Integer
million =
    Integer Positive [ 1000000 ]


billion : Integer
billion =
    Integer Positive [ 0, 100 ]


zillion : Integer
zillion =
    Integer Positive [ 0, 100000 ]



-- Basic Operations


add : Integer -> Integer -> Integer
add i1 i2 =
    case ( i1, i2 ) of
        ( Zero, _ ) ->
            i2

        ( _, Zero ) ->
            i1

        ( Integer Positive m1, Integer Negative m2 ) ->
            case compareMag m1 m2 of
                GT ->
                    Integer Positive (addMagnitudes m1 (List.map Basics.negate m2))

                EQ ->
                    Zero

                LT ->
                    Integer Negative (addMagnitudes (List.map Basics.negate m1) m2)

        ( Integer Negative m1, Integer Positive m2 ) ->
            case compareMag m1 m2 of
                GT ->
                    Integer Negative (addMagnitudes m1 (List.map Basics.negate m2))

                EQ ->
                    Zero

                LT ->
                    Integer Positive (addMagnitudes (List.map Basics.negate m1) m2)

        ( Integer s1 m1, Integer s2 m2 ) ->
            Integer s1 (addMagnitudes m1 m2)


sub : Integer -> Integer -> Integer
sub i1 i2 =
    case ( i1, i2 ) of
        ( Zero, _ ) ->
            negate i2

        ( _, Zero ) ->
            i1

        ( Integer s1 m1, Integer s2 m2 ) ->
            add i1 (negate i2)



-- assume abs m1 is greater than abs m2, so the most significant digit will never be negative


addMagnitudes : Magnitude -> Magnitude -> Magnitude
addMagnitudes m1 m2 =
    normalizeMagnitude (addMagnitudes_ m1 m2 [])


addMagnitudes_ : Magnitude -> Magnitude -> Magnitude -> Magnitude
addMagnitudes_ m1 m2 acc =
    case ( m1, m2 ) of
        ( [], [] ) ->
            List.reverse acc

        ( [], _ ) ->
            (List.reverse acc)
                ++ m2

        ( _, [] ) ->
            (List.reverse acc)
                ++ m1

        ( d1 :: ds1, d2 :: ds2 ) ->
            addMagnitudes_ ds1 ds2 (d1 + d2 :: acc)


normalizeMagnitude : Magnitude -> Magnitude
normalizeMagnitude m =
    trimLeadingZeroFromMag (List.reverse (handleFinalCarry (List.foldl normalizeDigit ( 0, [] ) m)))


normalizeDigit : Digit -> ( Digit, Magnitude ) -> ( Digit, Magnitude )
normalizeDigit d ( prevCarry, acc ) =
    let
        sum =
            (d + prevCarry)

        carry =
            if sum < 0 then
                -1
            else
                sum // defaultBase

        d_ =
            sum % defaultBase
    in
        ( carry, d_ :: acc )


handleFinalCarry : ( Digit, Magnitude ) -> Magnitude
handleFinalCarry ( c, m ) =
    if c == 0 then
        m
    else
        case m of
            -- this branch should not be reached
            [] ->
                []

            d :: ds ->
                if d + c == 0 then
                    ds
                else
                    c :: m


mul : Integer -> Integer -> Integer
mul i1 i2 =
    case ( i1, i2 ) of
        ( Zero, _ ) ->
            Zero

        ( _, Zero ) ->
            Zero

        ( Integer s1 m1, Integer s2 m2 ) ->
            let
                sign =
                    if s1 == s2 then
                        Positive
                    else
                        Negative

                magnitude =
                    case Basics.compare (List.length m1) (List.length m2) of
                        GT ->
                            multiplyMagnitudes m1 m2

                        _ ->
                            multiplyMagnitudes m2 m1
            in
                Integer sign magnitude



-- assume that length of m1 is longer than or equal to that of m2


multiplyMagnitudes : Magnitude -> Magnitude -> Magnitude
multiplyMagnitudes m1 m2 =
    normalizeMagnitude (sumPartialProducts (addScaleToPartialProducts (calculatePartialProducts m1 m2)))


calculatePartialProducts : Magnitude -> Magnitude -> List Magnitude
calculatePartialProducts m1 m2 =
    List.map (\d -> List.map ((*) d) m1) m2


addScaleToPartialProducts : List Magnitude -> List Magnitude
addScaleToPartialProducts magList =
    Tuple.second
        (List.foldl
            (\m ( digit, acc ) ->
                ( digit + 1
                , (List.append (List.repeat digit 0) m)
                    :: acc
                )
            )
            ( 0, [] )
            magList
        )


sumPartialProducts : List Magnitude -> Magnitude
sumPartialProducts magList =
    List.foldl addMagnitudes [] magList


div : Integer -> Integer -> Maybe Integer
div dividend divisor =
    divmod dividend divisor
        |> Maybe.map Tuple.first


unsafeDiv : Integer -> Integer -> Integer
unsafeDiv dividend divisor =
    divmod dividend divisor
        |> Maybe.map Tuple.first
        |> Maybe.withDefault (Zero)


rem : Integer -> Integer -> Maybe Integer
rem dividend divisor =
    divmod dividend divisor
        |> Maybe.map Tuple.second


unsafeRem : Integer -> Integer -> Integer
unsafeRem dividend divisor =
    divmod dividend divisor
        |> Maybe.map Tuple.second
        |> Maybe.withDefault (Zero)


divmod : Integer -> Integer -> Maybe ( Integer, Integer )
divmod dividend divisor =
    case ( dividend, divisor ) of
        ( Zero, _ ) ->
            Just ( Zero, Zero )

        ( _, Zero ) ->
            Nothing

        ( _, Integer Positive [ 1 ] ) ->
            Just ( dividend, Zero )

        ( _, Integer Negative [ 1 ] ) ->
            Just ( negate dividend, Zero )

        ( Integer s1 m1, Integer s2 m2 ) ->
            case compareMag m1 m2 of
                LT ->
                    Just ( Zero, dividend )

                EQ ->
                    let
                        sign =
                            if s1 == s2 then
                                Positive
                            else
                                Negative
                    in
                        Just ( Integer sign [ 1 ], Zero )

                GT ->
                    divmod_ (abs dividend) (abs divisor) Zero Zero
                        |> Maybe.map (adjustSign dividend divisor)


adjustSign : Integer -> Integer -> ( Integer, Integer ) -> ( Integer, Integer )
adjustSign dividend divisor ( q, r ) =
    case ( dividend, divisor ) of
        ( Integer Positive _, Integer Negative _ ) ->
            ( negate q, r )

        ( Integer Negative _, Integer Positive _ ) ->
            ( negate q, negate r )

        ( Integer Negative _, Integer Negative _ ) ->
            ( q, negate r )

        _ ->
            ( q, r )


unsafeDivmod : Integer -> Integer -> ( Integer, Integer )
unsafeDivmod dividend divisor =
    divmod dividend divisor
        |> Maybe.withDefault ( Zero, Zero )


divmod_ : Integer -> Integer -> Integer -> Integer -> Maybe ( Integer, Integer )
divmod_ dividend divisor qAcc prevR =
    case ( dividend, divisor ) of
        ( Zero, _ ) ->
            Just ( qAcc, prevR )

        _ ->
            let
                ( firstDigit, remainingDigits ) =
                    headAndTail dividend

                currentDividend =
                    add firstDigit (shiftRightBy 1 prevR)

                ( q, rem ) =
                    divmodPartialDividend currentDividend divisor defaultBase Zero

                qAcc_ =
                    add q (shiftRightBy 1 qAcc)
            in
                divmod_ remainingDigits divisor qAcc_ rem


headAndTail : Integer -> ( Integer, Integer )
headAndTail i =
    case i of
        Zero ->
            ( Zero, Zero )

        Integer s m ->
            let
                rM =
                    List.reverse m
            in
                case rM of
                    [] ->
                        ( Zero, Zero )

                    d :: [] ->
                        ( fromInt d, Zero )

                    d :: ds ->
                        ( fromInt d, Integer s (List.reverse ds) )


shiftRightBy : Int -> Integer -> Integer
shiftRightBy n i =
    case i of
        Zero ->
            Zero

        Integer s m ->
            if n <= 0 then
                i
            else
                shiftRightBy (n - 1) (Integer s (0 :: m))


divmodPartialDividend : Integer -> Integer -> Digit -> Integer -> ( Integer, Integer )
divmodPartialDividend dividend divisor divExpediter acc =
    case compare dividend divisor of
        LT ->
            ( acc, dividend )

        EQ ->
            ( add acc one, Zero )

        GT ->
            let
                divisorTimesDivExpediter =
                    mul divisor (fromInt divExpediter)
            in
                case compare dividend divisorTimesDivExpediter of
                    LT ->
                        divmodPartialDividend dividend divisor (divExpediter // 2) acc

                    EQ ->
                        ( add acc (fromInt divExpediter), Zero )

                    GT ->
                        let
                            dividend_ =
                                sub dividend divisorTimesDivExpediter
                        in
                            divmodPartialDividend dividend_ divisor divExpediter (add acc (fromInt divExpediter))



-- String representation


toString : Integer -> String
toString i =
    case i of
        Zero ->
            "0"

        Integer s m ->
            let
                sign =
                    if s == Negative then
                        "-"
                    else
                        ""

                num =
                    trimLeadingZeroFromStr (List.foldl (++) "" (List.map (Basics.toString >> (String.padLeft 7 '0')) m))
            in
                sign ++ num


trimLeadingZeroFromStr : String -> String
trimLeadingZeroFromStr =
    Regex.replace Regex.All (Regex.regex "^0*") (\_ -> "")



-- Sign modification functions


negate : Integer -> Integer
negate i =
    case i of
        Zero ->
            Zero

        Integer Positive m ->
            Integer Negative m

        Integer Negative m ->
            Integer Positive m


abs : Integer -> Integer
abs i =
    case i of
        Zero ->
            Zero

        Integer Positive _ ->
            i

        Integer Negative m ->
            Integer Positive m



-- Comparison functions


compare : Integer -> Integer -> Order
compare i1 i2 =
    case ( i1, i2 ) of
        ( Zero, Zero ) ->
            EQ

        ( Zero, Integer Positive _ ) ->
            LT

        ( Zero, Integer Negative _ ) ->
            GT

        ( Integer Positive _, Zero ) ->
            GT

        ( Integer Negative _, Zero ) ->
            LT

        ( Integer Positive _, Integer Negative _ ) ->
            GT

        ( Integer Negative _, Integer Positive _ ) ->
            LT

        ( Integer s1 m1, Integer s2 m2 ) ->
            let
                ord =
                    compareMag m1 m2
            in
                if s1 == Negative && s2 == Negative then
                    reverseOrder ord
                else
                    ord


reverseOrder : Order -> Order
reverseOrder o =
    case o of
        GT ->
            LT

        EQ ->
            EQ

        LT ->
            GT


lt : Integer -> Integer -> Bool
lt i1 i2 =
    case compare i1 i2 of
        LT ->
            True

        _ ->
            False


gt : Integer -> Integer -> Bool
gt i1 i2 =
    case compare i1 i2 of
        GT ->
            True

        _ ->
            False


lte : Integer -> Integer -> Bool
lte i1 i2 =
    case compare i1 i2 of
        LT ->
            True

        EQ ->
            True

        _ ->
            False


gte : Integer -> Integer -> Bool
gte i1 i2 =
    case compare i1 i2 of
        GT ->
            True

        EQ ->
            True

        _ ->
            False


eq : Integer -> Integer -> Bool
eq i1 i2 =
    case compare i1 i2 of
        EQ ->
            True

        _ ->
            False



-- Shared helper functions


compareMag : Magnitude -> Magnitude -> Order
compareMag m1 m2 =
    case Basics.compare (List.length m1) (List.length m2) of
        GT ->
            GT

        LT ->
            LT

        EQ ->
            compareMag_ (List.reverse m1) (List.reverse m2)


compareMag_ : Magnitude -> Magnitude -> Order
compareMag_ m1 m2 =
    case ( m1, m2 ) of
        ( [], [] ) ->
            EQ

        ( d :: ds, [] ) ->
            GT

        ( [], d :: ds ) ->
            LT

        ( d1 :: ds1, d2 :: ds2 ) ->
            case Basics.compare d1 d2 of
                GT ->
                    GT

                EQ ->
                    compareMag_ ds1 ds2

                LT ->
                    LT


trimLeadingZeroFromMag : Magnitude -> Magnitude
trimLeadingZeroFromMag =
    List.foldr
        (\x xs ->
            if (x == 0) && List.isEmpty xs then
                []
            else
                x :: xs
        )
        []

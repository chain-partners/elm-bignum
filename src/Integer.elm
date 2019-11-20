module Integer exposing
    ( Integer(..)
    , fromInt
    , fromString
    , zero
    , one
    , ten
    , hundred
    , thousand
    , million
    , billion
    , trillion
    , add
    , sub
    , mul
    , divmod
    , div
    , remainderBy
    , toString
    , negate
    , abs
    , compare
    , lt
    , gt
    , lte
    , gte
    , eq
    , countDigits
    )

{-| This library provides arbitrary precision `Integer` type and basic arithmetic operations on it.


# Definition

@docs Integer


# Constructors

`Integer` can be constructed from `Int` or `String`. Several convenience constructors for commonly used numbers are also provided.

@docs fromInt
@docs fromString
@docs zero
@docs one
@docs ten
@docs hundred
@docs thousand
@docs million
@docs billion
@docs trillion


# Basic Arithmetic

@docs add
@docs sub
@docs mul
@docs divmod
@docs div
@docs remainderBy


# String Representation

@docs toString


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


# Other Functions

@docs countDigits

-}

import Regex



-- Types


{-| An `Integer` is an arbitrary-precision number where its 'digits' of base 10 ^ 7 are represented as a linked list in little endian order. This base is chosen to simplify multiplication, toString, and debugging in Elm debugger.
-}
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


{-| Construct `Integer` from `Int`.

    fromInt 83748314374831 -- Integer Positive [4374831,8374831] : Integer

    fromInt 0 -- Zero : Integer

    fromInt -93939193 -- Integer Negative [3939193,9] : Integer

    Entering numbers that cannot be represented by Javascript number type will result in incorrect representation. Use fromString in that case.

    fromInt 81927398127398127389172893712893712893798123 |> toString -- "-10497034289617408" : String

    fromString "81927398127398127389172893712893712893798123" |> Maybe.map toString -- Just "81927398127398127389172893712893712893798123" : Maybe String

-}
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
        magnitudeFromInt_ (Basics.remainderBy defaultBase i :: acc) q


{-| Construct `Integer` from `String`.

    fromString "71289371892739812798734895701347589273948523984573" -- Just (Integer Positive [3984573,7394852,3475892,4895701,1279873,8927398,1289371,7]) : Maybe Integer

    fromString "jiaosdjf" -- Nothing : Maybe Integer

-}
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
    Regex.contains (Maybe.withDefault Regex.never (Regex.fromString "^-?\\d+$"))


magnitudeFromString : String -> Maybe Magnitude
magnitudeFromString s =
    combine (List.map String.toInt (splitFromEndBy 7 [] s))


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


{-| Convenience constructor for 0, equivalent to `fromInt 0`
-}
zero : Integer
zero =
    Zero


{-| Convenience constructor for 1, equivalent to `fromInt 1`
-}
one : Integer
one =
    Integer Positive [ 1 ]


{-| Convenience constructor for 10, equivalent to `fromInt 10`
-}
ten : Integer
ten =
    Integer Positive [ 10 ]


{-| Convenience constructor for one hundred, equivalent to `fromInt 100`
-}
hundred : Integer
hundred =
    Integer Positive [ 100 ]


{-| Convenience constructor for one thousand, equivalent to `fromInt 1000`
-}
thousand : Integer
thousand =
    Integer Positive [ 1000 ]


{-| Convenience constructor for one million, equivalent to `fromInt 1000000`
-}
million : Integer
million =
    Integer Positive [ 1000000 ]


{-| Convenience constructor for one billion, equivalent to `fromInt 1000000000`
-}
billion : Integer
billion =
    Integer Positive [ 0, 100 ]


{-| Convenience constructor for one trillion, equivalent to `fromInt 1000000000000`
-}
trillion : Integer
trillion =
    Integer Positive [ 0, 100000 ]



-- Basic Operations


{-| Add two `Integer`s.

    add ten million == Integer Positive [1000010] : Integer

-}
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


{-| Subtract two `Integer`s.

    sub million trillion == Integer Negative [9000000,99999] : Integer

-}
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
            List.reverse acc
                ++ m2

        ( _, [] ) ->
            List.reverse acc
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
            d + prevCarry

        carry =
            if sum < 0 then
                -1

            else
                sum // defaultBase

        d_ =
            modBy defaultBase sum
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


{-| Multiply two `Integer`s.

    mul (fromInt -873812381) (fromInt 78738732) == Integer Negative [5840892,287888,688] : Integer

-}
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
                , List.append (List.repeat digit 0) m
                    :: acc
                )
            )
            ( 0, [] )
            magList
        )


sumPartialProducts : List Magnitude -> Magnitude
sumPartialProducts magList =
    List.foldl addMagnitudes [] magList


{-| Divide two `Integer`s and get both quotient and remainder.

    divmod (fromInt -873812381) (fromInt 78738732) == Just (Integer Negative [11],Integer Negative [7686329]) : Maybe ( Integer, Integer )
    divmod zero (fromInt 871)                      == Just Zero : Maybe Integer
    divmod (fromInt 871) zero                      == Nothing : Maybe Integer

-}
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

                ( q, r ) =
                    divmodPartialDividend currentDividend divisor defaultBase Zero

                qAcc_ =
                    add q (shiftRightBy 1 qAcc)
            in
            divmod_ remainingDigits divisor qAcc_ r


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


{-| Divide two `Integer`s and get quotient.

    div (fromInt -873812381) (fromInt 78738732) == Just (Integer Negative [11]) : Maybe Integer
    div zero (fromInt 871)                      == Just Zero : Maybe Integer
    div (fromInt 871) zero                      == Nothing : Maybe Integer

-}
div : Integer -> Integer -> Maybe Integer
div dividend divisor =
    divmod dividend divisor
        |> Maybe.map Tuple.first


{-| Divide two `Integer`s and get remainder.

    remainderBy (fromInt -873812381) (fromInt 78738732) == Just (Integer Negative [7686329]) : Maybe Integer
    remainderBy zero (fromInt 871)                      == Just Zero : Maybe Integer
    remainderBy (fromInt 871) zero                      == Nothing : Maybe Integer

-}
remainderBy : Integer -> Integer -> Maybe Integer
remainderBy dividend divisor =
    divmod dividend divisor
        |> Maybe.map Tuple.second



-- String representation


{-| Represent `Integer` as `String`.

    mul (fromInt 8172387) (fromInt -332)
        |> toString == "-2713232484" : String

-}
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
                    trimLeadingZeroFromStr (List.foldl (++) "" (List.map (String.fromInt >> String.padLeft 7 '0') m))
            in
            sign ++ num


trimLeadingZeroFromStr : String -> String
trimLeadingZeroFromStr =
    Regex.replace (Maybe.withDefault Regex.never (Regex.fromString "^0*")) (\_ -> "")



-- Sign modification functions


{-| Negate the sign of `Integer`.

    negate (fromInt -332)  == Integer Positive [332] : Integer
    negate (fromInt 8872)  == Integer Negative [8872] : Integer
    negate zero            == Zero : Integer

-}
negate : Integer -> Integer
negate i =
    case i of
        Zero ->
            Zero

        Integer Positive m ->
            Integer Negative m

        Integer Negative m ->
            Integer Positive m


{-| Get absolute value of `Integer`.

    negate (fromInt -332)  == Integer Positive [332] : Integer
    negate (fromInt 8872)  == Integer Positive [8872] : Integer
    negate zero            == Zero : Integer

-}
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


{-| Compare two `Integer`s.

    compare one zero == GT

    compare one one == EQ

    compare (negate million) one == LT

-}
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



-- Integer length


{-| Count the number of decimal digits.

    mul (fromInt 8172387) (fromInt -332) |> countDigits == 10 : Int

-}
countDigits : Integer -> Int
countDigits i =
    let
        s =
            toString i
    in
    if String.startsWith "-" s then
        String.length (String.dropLeft 1 s)

    else
        String.length s



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

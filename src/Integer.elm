module Integer
    exposing
        ( Integer
        , fromInt
        , fromString
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
            Integer Positive (magnitudeFromInt i [])

        EQ ->
            Zero

        LT ->
            Integer Negative (magnitudeFromInt (Basics.abs i) [])


magnitudeFromInt : Int -> Magnitude -> Magnitude
magnitudeFromInt i acc =
    let
        q =
            i // defaultBase

        r =
            Basics.rem i defaultBase

        acc_ =
            r :: acc
    in
        if q == 0 then
            List.reverse acc_
        else
            magnitudeFromInt q acc_


fromString : String -> Maybe Integer
fromString s =
    s
        |> validateString
        |> Maybe.andThen fromString_


validateString : String -> Maybe String
validateString s =
    let
        hasValidChars : String -> Bool
        hasValidChars s =
            String.all (\c -> c == '-' || Char.isDigit c) s

        hasSignOnlyAtBeginning : String -> Bool
        hasSignOnlyAtBeginning s =
            (String.startsWith "-" s)
                || (s |> String.filter (\c -> c == '-') >> String.isEmpty)
    in
        if (hasValidChars s) && (hasSignOnlyAtBeginning s) then
            Just s
        else
            Nothing


fromString_ : String -> Maybe Integer
fromString_ s =
    let
        ( sign, num ) =
            if String.startsWith "-" s then
                ( Negative, s |> String.dropLeft 1 >> trimLeadingZeroFromStr )
            else
                ( Positive, trimLeadingZeroFromStr s )
    in
        if String.isEmpty num then
            Just Zero
        else
            Maybe.map (Integer sign) (magnitudeFromString num)


magnitudeFromString : String -> Maybe Magnitude
magnitudeFromString s =
    s
        |> splitFromEndBy 7 []
        |> List.map (String.toInt >> Result.toMaybe)
        |> combine


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
    let
        zipMagnitudes : Magnitude -> Magnitude -> List ( Digit, Digit ) -> List ( Digit, Digit )
        zipMagnitudes m1 m2 acc =
            case ( m1, m2 ) of
                ( [], [] ) ->
                    acc

                ( [], d :: ds ) ->
                    zipMagnitudes [] ds (( 0, d ) :: acc)

                ( d :: ds, [] ) ->
                    zipMagnitudes ds [] (( d, 0 ) :: acc)

                ( d1 :: ds1, d2 :: ds2 ) ->
                    zipMagnitudes ds1 ds2 (( d1, d2 ) :: acc)
    in
        zipMagnitudes m1 m2 []
            |> List.foldl (\( d1, d2 ) acc -> (d1 + d2) :: acc) []
            |> normalizeMagnitude


normalizeMagnitude : Magnitude -> Magnitude
normalizeMagnitude m =
    let
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
    in
        m
            |> List.foldl normalizeDigit ( 0, [] )
            |> handleFinalCarry
            |> List.reverse
            |> trimLeadingZeroFromMag


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
    let
        calculatePartialProducts : Magnitude -> Magnitude -> List Magnitude
        calculatePartialProducts m1 m2 =
            List.map (\d -> List.map ((*) d) m1) m2

        addScaleToPartialProducts : List Magnitude -> ( Int, List Magnitude )
        addScaleToPartialProducts magList =
            List.foldl
                (\m ( digit, acc ) ->
                    ( digit + 1
                    , (List.append (List.repeat digit 0) m)
                        :: acc
                    )
                )
                ( 0, [] )
                magList

        sumPartialProducts : List Magnitude -> Magnitude
        sumPartialProducts magList =
            List.foldl addMagnitudes [] magList
    in
        calculatePartialProducts m1 m2
            |> addScaleToPartialProducts
            |> Tuple.second
            |> sumPartialProducts
            |> normalizeMagnitude


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
    case ( getSign dividend, getSign divisor ) of
        ( Positive, Positive ) ->
            ( q, r )

        ( Positive, Negative ) ->
            ( negate q, r )

        ( Negative, Positive ) ->
            ( negate q, negate r )

        ( Negative, Negative ) ->
            ( q, negate r )


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
                ( d, ds ) =
                    headAndTail dividend

                dividend_ =
                    add d (shiftRightBy 1 prevR)

                ( q, rem ) =
                    divmodHelper dividend_ divisor defaultBase Zero

                qAcc_ =
                    add q (shiftRightBy 1 qAcc)
            in
                divmod_ ds divisor qAcc_ rem


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


divmodHelper : Integer -> Integer -> Digit -> Integer -> ( Integer, Integer )
divmodHelper dividend divisor divExpediter acc =
    case compare dividend divisor of
        LT ->
            ( acc, dividend )

        EQ ->
            ( add acc (fromInt 1), Zero )

        GT ->
            case compare dividend (mul divisor (fromInt divExpediter)) of
                LT ->
                    divmodHelper dividend divisor (divExpediter // 2) acc

                EQ ->
                    ( add acc (fromInt divExpediter), Zero )

                GT ->
                    let
                        dividend_ =
                            sub dividend (mul divisor (fromInt divExpediter))
                    in
                        divmodHelper dividend_ divisor divExpediter (add acc (fromInt divExpediter))



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
                    m
                        |> List.map (Basics.toString >> (String.padLeft 7 '0'))
                        |> List.foldl (++) ""
                        |> trimLeadingZeroFromStr
            in
                sign ++ num


trimLeadingZeroFromStr : String -> String
trimLeadingZeroFromStr =
    String.foldl
        (\c cs ->
            if c == '0' && cs == "" then
                ""
            else
                String.cons c cs
        )
        ""
        >> String.reverse



-- Sign modification functions


negate : Integer -> Integer
negate i =
    case i of
        Zero ->
            Zero

        Integer s m ->
            Integer (negateSign s) m


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


negateSign : Sign -> Sign
negateSign s =
    case s of
        Positive ->
            Negative

        Negative ->
            Positive


trimLeadingZeroFromMag : Magnitude -> Magnitude
trimLeadingZeroFromMag m =
    dropWhileEnd ((==) 0) m


dropWhileEnd : (a -> Bool) -> List a -> List a
dropWhileEnd p =
    List.foldr
        (\x xs ->
            if p x && List.isEmpty xs then
                []
            else
                x :: xs
        )
        []


getSign : Integer -> Sign
getSign i =
    case i of
        Integer Negative _ ->
            Negative

        _ ->
            Positive

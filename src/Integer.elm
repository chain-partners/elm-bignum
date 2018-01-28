module Integer
    exposing
        ( Integer
        , fromInt
        , fromString
        , add
        , sub
        , mul
        , divmod
        , safeDivmod
        , div
        , safeDiv
        , rem
        , safeRem
        , abs
        , negate
        , compare
        , lt
        , lte
        , gt
        , gte
        , eq
        , toString
        , trimLeadingZeroFromMag
        , trimLeadingZeroFromStr
        , normalizeMagnitude
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


maxBase : Base
maxBase =
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
        quotient =
            i // maxBase

        rem =
            Basics.rem i maxBase

        acc_ =
            rem :: acc
    in
        if quotient == 0 then
            List.reverse acc_
        else
            magnitudeFromInt quotient acc_


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

        hasSignAtStart : String -> Bool
        hasSignAtStart s =
            (String.indices "-" s)
                == [ 0 ]
                || (s |> String.filter (\c -> c == '-') |> String.isEmpty)
    in
        if (hasValidChars s) && (hasSignAtStart s) then
            Just s
        else
            Nothing


fromString_ : String -> Maybe Integer
fromString_ s =
    let
        ( sign, num ) =
            if String.startsWith "-" s then
                ( Negative, s |> String.dropLeft 1 |> trimLeadingZeroFromStr )
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
                    Integer Positive (addMagnitudes m1 (List.map Basics.negate m2) [] |> normalizeMagnitude)

                EQ ->
                    Zero

                LT ->
                    Integer Negative (addMagnitudes (List.map Basics.negate m1) m2 [] |> normalizeMagnitude)

        ( Integer Negative m1, Integer Positive m2 ) ->
            case compareMag m1 m2 of
                GT ->
                    Integer Negative (addMagnitudes m1 (List.map Basics.negate m2) [] |> normalizeMagnitude)

                EQ ->
                    Zero

                LT ->
                    Integer Positive (addMagnitudes (List.map Basics.negate m1) m2 [] |> normalizeMagnitude)

        ( Integer s1 m1, Integer s2 m2 ) ->
            Integer s1 (addMagnitudes m1 m2 [] |> normalizeMagnitude)


addMagsWithCarry : Magnitude -> Magnitude -> Magnitude -> Digit -> Magnitude
addMagsWithCarry m1 m2 acc prevCarry =
    case ( m1, m2 ) of
        ( [], [] ) ->
            if prevCarry == 0 then
                acc
                    |> List.reverse
                    |> trimLeadingZeroFromMag
            else
                prevCarry
                    :: acc
                    |> List.reverse
                    |> trimLeadingZeroFromMag

        ( [], d :: ds ) ->
            let
                sum =
                    d + prevCarry

                carry =
                    sum // maxBase

                rem =
                    Basics.rem sum maxBase
            in
                addMagsWithCarry [] ds (rem :: acc) carry

        ( d :: ds, [] ) ->
            let
                sum =
                    d + prevCarry

                carry =
                    sum // maxBase

                rem =
                    Basics.rem sum maxBase
            in
                addMagsWithCarry ds [] (rem :: acc) carry

        ( d1 :: ds1, d2 :: ds2 ) ->
            let
                sum =
                    d1 + d2 + prevCarry

                carry =
                    sum // maxBase

                rem =
                    Basics.rem sum maxBase
            in
                addMagsWithCarry ds1 ds2 (rem :: acc) carry


sub : Integer -> Integer -> Integer
sub i1 i2 =
    case ( i1, i2 ) of
        ( Zero, _ ) ->
            negate i2

        ( _, Zero ) ->
            i1

        ( Integer s1 m1, Integer s2 m2 ) ->
            add i1 (negate i2)


normalizeMagnitude : Magnitude -> Magnitude
normalizeMagnitude m =
    let
        folder : Digit -> ( Digit, Magnitude ) -> ( Digit, Magnitude )
        folder d ( prevCarry, acc ) =
            let
                sum =
                    (d + prevCarry)

                carry =
                    if sum < 0 then
                        -1
                    else
                        sum // maxBase

                d_ =
                    sum % maxBase
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
                            (d + c) :: ds
    in
        m
            |> List.foldl folder ( 0, [] )
            |> handleFinalCarry
            |> List.reverse
            |> trimLeadingZeroFromMag


addMagnitudes : Magnitude -> Magnitude -> Magnitude -> Magnitude
addMagnitudes m1 m2 acc =
    case ( m1, m2 ) of
        ( [], [] ) ->
            List.reverse acc

        ( [], _ ) ->
            List.append (List.reverse acc) m2

        ( _, [] ) ->
            List.append (List.reverse acc) m1

        ( d1 :: ds1, d2 :: ds2 ) ->
            addMagnitudes ds1 ds2 ((d1 + d2) :: acc)


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
                            mulMagsWithCarry m1 m2 [] []

                        _ ->
                            mulMagsWithCarry m2 m1 [] []
            in
                Integer sign magnitude


mulMagsWithCarry : Magnitude -> Magnitude -> Magnitude -> Magnitude -> Magnitude
mulMagsWithCarry m1 m2 acc prevCarry =
    case ( m1, m2 ) of
        ( _, [] ) ->
            if prevCarry == [] then
                List.reverse acc
            else
                List.reverse ((List.reverse prevCarry) ++ acc)

        ( _, d :: ds ) ->
            let
                product =
                    addMagsWithCarry (mulMagWithOneDigit m1 d [] 0) prevCarry [] 0

                rem =
                    product
                        |> List.head
                        |> Maybe.withDefault 0

                carry =
                    product
                        |> List.tail
                        |> Maybe.withDefault []
            in
                mulMagsWithCarry m1 ds (rem :: acc) carry


mulMagWithOneDigit : Magnitude -> Digit -> Magnitude -> Digit -> Magnitude
mulMagWithOneDigit m multiplier acc prevCarry =
    case m of
        [] ->
            if prevCarry == 0 then
                List.reverse acc
            else
                List.reverse (prevCarry :: acc)

        d :: ds ->
            let
                product =
                    d * multiplier + prevCarry

                carry =
                    product // maxBase

                rem =
                    product % maxBase
            in
                mulMagWithOneDigit ds multiplier (rem :: acc) carry


safeDiv : Integer -> Integer -> Maybe Integer
safeDiv dividend divisor =
    safeDivmod dividend divisor
        |> Maybe.map Tuple.first


div : Integer -> Integer -> Integer
div dividend divisor =
    safeDivmod dividend divisor
        |> Maybe.map Tuple.first
        |> Maybe.withDefault (Zero)


safeRem : Integer -> Integer -> Maybe Integer
safeRem dividend divisor =
    safeDivmod dividend divisor
        |> Maybe.map Tuple.second


rem : Integer -> Integer -> Integer
rem dividend divisor =
    safeDivmod dividend divisor
        |> Maybe.map Tuple.second
        |> Maybe.withDefault (Zero)


safeDivmod : Integer -> Integer -> Maybe ( Integer, Integer )
safeDivmod dividend divisor =
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


divmod : Integer -> Integer -> ( Integer, Integer )
divmod dividend divisor =
    safeDivmod dividend divisor
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
                    divmodHelper dividend_ divisor maxBase Zero

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

                    rd :: [] ->
                        ( fromInt rd, Zero )

                    rd :: rds ->
                        ( fromInt rd, Integer s (List.reverse rds) )


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
divmodHelper dividend divisor normalizer acc =
    case compare dividend divisor of
        LT ->
            ( acc, dividend )

        EQ ->
            ( add acc (fromInt 1), Zero )

        GT ->
            case compare dividend (mul divisor (fromInt normalizer)) of
                LT ->
                    divmodHelper dividend divisor (normalizer // 2) acc

                EQ ->
                    ( add acc (fromInt normalizer), Zero )

                GT ->
                    let
                        dividend_ =
                            sub dividend (mul divisor (fromInt normalizer))
                    in
                        divmodHelper dividend_ divisor normalizer (add acc (fromInt normalizer))



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
                        |> List.map Basics.toString
                        |> List.map (String.padLeft 7 '0')
                        |> List.foldl (++) ""
                        |> trimLeadingZeroFromStr
            in
                sign ++ num


trimLeadingZeroFromStr : String -> String
trimLeadingZeroFromStr s =
    String.foldl
        (\c cs ->
            if c == '0' && cs == "" then
                ""
            else
                String.cons c cs
        )
        ""
        s
        |> String.reverse



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

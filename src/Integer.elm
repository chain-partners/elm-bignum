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
        )

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


type alias BigEndianMagnitude =
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
            Integer Positive (magnitudeFromInt i)

        EQ ->
            Zero

        LT ->
            Integer Negative (magnitudeFromInt (Basics.abs i))


magnitudeFromInt : Int -> Magnitude
magnitudeFromInt i =
    if i < maxBase then
        [ i ]
    else
        let
            quotient =
                i // maxBase

            rem =
                i % maxBase
        in
            rem :: magnitudeFromInt quotient


fromString : String -> Maybe Integer
fromString s =
    if String.startsWith "-" s then
        let
            s_ =
                String.dropLeft 1 s

            m =
                magnitudeFromString s_
        in
            Maybe.map (Integer Negative) m
    else
        let
            m =
                magnitudeFromString s
        in
            Maybe.map (Integer Positive) m


magnitudeFromString : String -> Maybe Magnitude
magnitudeFromString s =
    s
        |> String.reverse
        |> splitBy 7 []
        |> List.map String.reverse
        |> List.map (String.toInt >> Result.toMaybe)
        |> combine


splitBy : Int -> List String -> String -> List String
splitBy n acc s =
    if s == "" then
        acc
    else
        let
            chunk =
                String.left n s

            rest =
                String.dropLeft n s
        in
            splitBy n (chunk :: acc) rest


combine : List (Maybe Int) -> Maybe (List Int)
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
            sub i1 (abs i2)

        ( Integer Negative m1, Integer Positive m2 ) ->
            sub i2 (abs i1)

        ( Integer s1 m1, Integer s2 m2 ) ->
            Integer s1 (addMagsWithCarry m1 m2 [] 0)


addMagsWithCarry : Magnitude -> Magnitude -> Magnitude -> Digit -> Magnitude
addMagsWithCarry m1 m2 acc prevCarry =
    case ( m1, m2 ) of
        ( [], [] ) ->
            if prevCarry == 0 then
                removeLeadingZero acc
            else
                removeLeadingZero (prevCarry :: acc)

        ( [], d :: ds ) ->
            let
                sum =
                    d + prevCarry

                carry =
                    if sum >= maxBase then
                        1
                    else
                        0

                rem =
                    sum % maxBase
            in
                addMagsWithCarry [] ds (rem :: acc) carry

        ( d :: ds, [] ) ->
            let
                sum =
                    d + prevCarry

                carry =
                    if sum >= maxBase then
                        1
                    else
                        0

                rem =
                    sum % maxBase
            in
                addMagsWithCarry ds [] (rem :: acc) carry

        ( d1 :: ds1, d2 :: ds2 ) ->
            let
                sum =
                    d1 + d2 + prevCarry

                carry =
                    if sum >= maxBase then
                        1
                    else
                        0

                rem =
                    sum % maxBase
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
            if s1 /= s2 then
                Integer s1 (addMagsWithCarry m1 m2 [] 0)
            else
                case compareMag m1 m2 of
                    GT ->
                        Integer s1 (subMagsWithCarry m1 m2 [] 0)

                    EQ ->
                        Zero

                    LT ->
                        Integer (negateSign s2) (subMagsWithCarry m2 m1 [] 0)


subMagsWithCarry : Magnitude -> Magnitude -> Magnitude -> Digit -> Magnitude
subMagsWithCarry m1 m2 acc prevCarry =
    case ( m1, m2 ) of
        ( [], [] ) ->
            if prevCarry == 0 then
                removeLeadingZero acc
            else
                removeLeadingZero
                    (prevCarry :: acc)

        ( [], d :: ds ) ->
            let
                diff =
                    d - prevCarry

                carry =
                    if diff < 0 then
                        1
                    else
                        0

                rem =
                    diff % maxBase
            in
                subMagsWithCarry [] ds (rem :: acc) carry

        ( d :: ds, [] ) ->
            let
                diff =
                    d - prevCarry

                carry =
                    if diff < 0 then
                        1
                    else
                        0

                rem =
                    diff % maxBase
            in
                subMagsWithCarry ds [] (rem :: acc) carry

        ( d1 :: ds1, d2 :: ds2 ) ->
            let
                diff =
                    d1 - d2 - prevCarry

                carry =
                    if diff < 0 then
                        1
                    else
                        0

                rem =
                    diff % maxBase
            in
                subMagsWithCarry ds1 ds2 (rem :: acc) carry


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
                    let
                        adjustRemainder : Integer -> Integer -> Integer
                        adjustRemainder divisor rem =
                            if s1 == s2 then
                                rem
                            else
                                add (abs divisor) rem
                    in
                        divmod_ (abs dividend) (abs divisor) Zero Zero
                            |> Maybe.map (Tuple.mapSecond (adjustRemainder divisor))


divmod : Integer -> Integer -> ( Integer, Integer )
divmod dividend divisor =
    safeDivmod dividend divisor
        |> Maybe.withDefault ( Zero, Zero )


divmod_ : Integer -> Integer -> Integer -> Integer -> Maybe ( Integer, Integer )
divmod_ dividend divisor qAcc prevR =
    if dividend == Zero then
        Just ( qAcc, prevR )
    else
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
                        |> trimLeadingZero
            in
                sign ++ num


trimLeadingZero : String -> String
trimLeadingZero s =
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

        ( Integer _ m1, Integer _ m2 ) ->
            compareMag m1 m2


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


removeLeadingZero : Magnitude -> Magnitude
removeLeadingZero m =
    dropWhileEnd ((==) 0) (List.reverse m)


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

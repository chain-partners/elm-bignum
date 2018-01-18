module BigNum
    exposing
        ( Integer
        , fromInt
        , negate
        , abs
        , add
        , sub
        , mul
        , divmod
        , unsafeDivmod
        , div
        , unsafeDiv
        , rem
        , unsafeRem
        , compare
        )


type Sign
    = Positive
    | Negative


type alias Digit =
    Int


type alias Base =
    Int


type alias Magnitude =
    List Digit


type Integer
    = Integer Sign Magnitude
    | Zero


maxBase : Base
maxBase =
    2 ^ 26


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


getMag : Integer -> Magnitude
getMag i =
    case i of
        Zero ->
            [ 0 ]

        Integer _ m ->
            m


magLength : Integer -> Int
magLength i =
    case i of
        Zero ->
            1

        Integer _ m ->
            List.length m


takeDigit : Int -> Integer -> Integer
takeDigit n i =
    case i of
        Zero ->
            Zero

        Integer s m ->
            Integer s (List.take n m)


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


negateSign : Sign -> Sign
negateSign s =
    case s of
        Positive ->
            Negative

        Negative ->
            Positive


add : Integer -> Integer -> Integer
add i1 i2 =
    case ( i1, i2 ) of
        ( Zero, _ ) ->
            i2

        ( _, Zero ) ->
            i1

        ( Integer s1 m1, Integer s2 m2 ) ->
            if s1 == s2 then
                Integer s1 (addMagsWithCarry m1 m2 [] 0)
            else
                case compareMag m1 m2 of
                    GT ->
                        sub i1 i2

                    EQ ->
                        Zero

                    LT ->
                        sub i2 i1


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


compareMag : Magnitude -> Magnitude -> Order
compareMag m1 m2 =
    case Basics.compare (List.length m1) (List.length m2) of
        GT ->
            GT

        LT ->
            LT

        EQ ->
            let
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
            in
                compareMag_ (List.reverse m1) (List.reverse m2)


addMagsWithCarry : Magnitude -> Magnitude -> Magnitude -> Digit -> Magnitude
addMagsWithCarry m1 m2 acc prevCarry =
    case ( m1, m2 ) of
        ( [], [] ) ->
            if prevCarry == 0 then
                acc
                    |> removeLeadingZero
            else
                (prevCarry :: acc)
                    |> removeLeadingZero

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
                acc
                    |> removeLeadingZero
            else
                (prevCarry :: acc)
                    |> removeLeadingZero

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
                    List.head product |> Maybe.withDefault 0

                carry =
                    List.tail product |> Maybe.withDefault []
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
                    divmod_ dividend divisor Zero Zero


unsafeDivmod : Integer -> Integer -> ( Integer, Integer )
unsafeDivmod dividend divisor =
    divmod dividend divisor
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



{-
   toString :
   1. convert to base10
   2. concat string

   toString : Integer -> String
   toString i =
       i
       |> convertToBase10
       |> concatString

   convertToBase10 : Integer -> Integer
   convertToBase10 i =
       case i of
           Zero -> Zero
           Integer s m ->

-}

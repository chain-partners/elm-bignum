module BigNum exposing (Integer, fromInt, negate, add, subtract, multiply)


type Sign
    = Positive
    | Negative


type alias Digit =
    Int


type alias Magnitude =
    List Digit


type Integer
    = Integer Sign Magnitude
    | Zero


maxDigitValue : Digit
maxDigitValue =
    2 ^ 26


fromInt : Int -> Integer
fromInt i =
    case compare i 0 of
        GT ->
            Integer Positive (magnitudeFromInt i)

        EQ ->
            Zero

        LT ->
            Integer Negative (magnitudeFromInt (abs i))


magnitudeFromInt : Int -> Magnitude
magnitudeFromInt i =
    if i < maxDigitValue then
        [ i ]
    else
        let
            quotient =
                i // maxDigitValue

            rem =
                i % maxDigitValue
        in
            rem :: magnitudeFromInt quotient


negate : Integer -> Integer
negate i =
    case i of
        Zero ->
            Zero

        Integer s m ->
            Integer (negateSign s) m


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
                        subtract i1 i2

                    EQ ->
                        Zero

                    LT ->
                        subtract i2 i1


compareMag : Magnitude -> Magnitude -> Order
compareMag m1 m2 =
    case compare (List.length m1) (List.length m2) of
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
                            case compare d1 d2 of
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
                List.reverse acc
            else
                List.reverse (prevCarry :: acc)

        ( [], d :: ds ) ->
            let
                sum =
                    d + prevCarry

                carry =
                    if sum >= maxDigitValue then
                        1
                    else
                        0

                rem =
                    sum % maxDigitValue
            in
                addMagsWithCarry [] ds (rem :: acc) carry

        ( d :: ds, [] ) ->
            let
                sum =
                    d + prevCarry

                carry =
                    if sum >= maxDigitValue then
                        1
                    else
                        0

                rem =
                    sum % maxDigitValue
            in
                addMagsWithCarry ds [] (rem :: acc) carry

        ( d1 :: ds1, d2 :: ds2 ) ->
            let
                sum =
                    d1 + d2 + prevCarry

                carry =
                    if sum >= maxDigitValue then
                        1
                    else
                        0

                rem =
                    sum % maxDigitValue
            in
                addMagsWithCarry ds1 ds2 (rem :: acc) carry


subtract : Integer -> Integer -> Integer
subtract i1 i2 =
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
                List.reverse acc
            else
                List.reverse (prevCarry :: acc)

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
                    diff % maxDigitValue
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
                    diff % maxDigitValue
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
                    diff % maxDigitValue
            in
                subMagsWithCarry ds1 ds2 (rem :: acc) carry


multiply : Integer -> Integer -> Integer
multiply i1 i2 =
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
                    case compare (List.length m1) (List.length m2) of
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
                    addMagsWithCarry (mulMagWithDigit m1 d [] 0) prevCarry [] 0

                rem =
                    List.head product |> Maybe.withDefault 0

                carry =
                    List.tail product |> Maybe.withDefault []
            in
                mulMagsWithCarry m1 ds (rem :: acc) carry


mulMagWithDigit : Magnitude -> Digit -> Magnitude -> Digit -> Magnitude
mulMagWithDigit m multiplier acc prevCarry =
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
                    product // maxDigitValue

                rem =
                    product % maxDigitValue
            in
                mulMagWithDigit ds multiplier (rem :: acc) carry

module W.Chart.Internal.Scale exposing
    ( normalize
    , normalizeDomains
    )

import Scale


normalize :
    ( ( Float, Float ) -> ( Float, Float ) -> Scale.ContinuousScale Float, Scale.ContinuousScale Float )
    -> ( ( Float, Float ) -> ( Float, Float ) -> Scale.ContinuousScale Float, Scale.ContinuousScale Float )
    -> ( Scale.ContinuousScale Float, Scale.ContinuousScale Float )
normalize ( fnA, a ) ( fnB, b ) =
    let
        ( aNormalized, bNormalized ) =
            normalizeDomains (Scale.domain a) (Scale.domain b)
    in
    ( fnA (Scale.range a) aNormalized
    , fnB (Scale.range b) bNormalized
    )


normalizeDomains :
    ( Float, Float )
    -> ( Float, Float )
    -> ( ( Float, Float ), ( Float, Float ) )
normalizeDomains a b =
    let
        aDomain : ( Float, Float )
        aDomain =
            toZeroDomain a

        bDomain : ( Float, Float )
        bDomain =
            toZeroDomain b

        delta : Float
        delta =
            Basics.max
                (toDomainDelta aDomain)
                (toDomainDelta bDomain)
    in
    if delta == 0 then
        ( a, b )

    else
        ( normalizeDomain delta aDomain
        , normalizeDomain delta bDomain
        )


toZeroDomain : ( Float, Float ) -> ( Float, Float )
toZeroDomain =
    Tuple.mapBoth
        (Basics.min 0)
        (Basics.max 0)


toDomainDelta : ( Float, Float ) -> Float
toDomainDelta ( min, max ) =
    safeDivide (abs min) (abs max)


normalizeDomain : Float -> ( Float, Float ) -> ( Float, Float )
normalizeDomain delta ( min, max ) =
    ( max * delta * -1
    , max
    )


safeDivide : Float -> Float -> Float
safeDivide x y =
    if y == 0.0 then
        0.0

    else
        x / y

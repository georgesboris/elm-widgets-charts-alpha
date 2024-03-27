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
                |> Debug.log "aDomain"

        bDomain : ( Float, Float )
        bDomain =
            toZeroDomain b
                |> Debug.log "bDomain"

        delta : Float
        delta =
            Basics.max
                (toDomainDelta aDomain)
                (toDomainDelta bDomain)

        hasNegativeOnlyDomain : Bool
        hasNegativeOnlyDomain =
            isInfinite delta
    in
    if not hasNegativeOnlyDomain then
        ( normalizeDomain delta aDomain
        , normalizeDomain delta bDomain
        )

    else if Tuple.second aDomain > 0.0 || Tuple.second bDomain > 0.0 then
        ( centralizeDomain aDomain
        , centralizeDomain bDomain
        )

    else
        ( aDomain
        , bDomain
        )


domainsAreOpposites : ( Float, Float ) -> ( Float, Float ) -> Bool
domainsAreOpposites ( aMin, aMax ) ( bMin, bMax ) =
    (aMin == 0.0 && aMax >= 0.0 && bMax == 0.0 && bMin <= 0.0)
        || (bMin == 0.0 && bMax >= 0.0 && aMax == 0.0 && aMin <= 0.0)


toZeroDomain : ( Float, Float ) -> ( Float, Float )
toZeroDomain =
    Tuple.mapBoth
        (Basics.min 0)
        (Basics.max 0)


toDomainDelta : ( Float, Float ) -> Float
toDomainDelta ( min, max ) =
    abs (safeDivide min max)


normalizeDomain : Float -> ( Float, Float ) -> ( Float, Float )
normalizeDomain delta ( min, max ) =
    ( max * delta * -1
    , max
    )


centralizeDomain : ( Float, Float ) -> ( Float, Float )
centralizeDomain ( min, max ) =
    let
        absMax : Float
        absMax =
            Basics.max (abs min) max
    in
    ( absMax * -1
    , absMax
    )


safeDivide : Float -> Float -> Float
safeDivide x y =
    let
        result : Float
        result =
            x / y
    in
    if isNaN result then
        0.0

    else
        result

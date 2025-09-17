module W.Chart.Widget.Label exposing
    ( view, viewList, viewBinsList
    , inside, centered, withStroke
    , format, formatAsPercentage, formatWithList
    , formatStack
    , Attribute
    )

{-|

@docs view, viewList, viewBinsList
@docs inside, centered, withStroke
@docs format, formatAsPercentage, formatWithList
@docs formatStack
@docs Attribute

-}

import Attr
import Html as H
import Html.Attributes as HA
import Scale
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC
import TypedSvg.Types as ST
import W.Chart
import W.Chart.Internal
import W.Theme.Color



-- Attributes


{-| -}
type alias Attribute =
    Attr.Attr Attributes


type alias Attributes =
    { position : Position
    , stroke : Bool
    , format : Maybe (List Float -> Float -> String)
    , formatStack : Maybe (List Float -> String)
    }


defaultAttrs : Attributes
defaultAttrs =
    { position = Outside
    , stroke = False
    , format = Nothing
    , formatStack = Nothing
    }


type Position
    = Outside
    | Inside
    | Center


{-| -}
inside : Attribute
inside =
    Attr.attr (\attr -> { attr | position = Inside })


{-| -}
centered : Attribute
centered =
    Attr.attr (\attr -> { attr | position = Center })


{-| -}
withStroke : Attribute
withStroke =
    Attr.attr (\attr -> { attr | stroke = True })


{-| -}
formatStack : (List Float -> String) -> Attribute
formatStack fn =
    Attr.attr (\attr -> { attr | formatStack = Just fn })


{-| -}
format : (Float -> String) -> Attribute
format fn =
    Attr.attr (\attr -> { attr | format = Just (\_ -> fn) })


{-| -}
formatWithList : (List Float -> Float -> String) -> Attribute
formatWithList fn =
    Attr.attr (\attr -> { attr | format = Just fn })


{-| -}
formatAsPercentage : Attribute
formatAsPercentage =
    Attr.attr
        (\attr ->
            { attr
                | format =
                    Just
                        (\xs ->
                            let
                                total : Float
                                total =
                                    xs
                                        |> List.map Basics.abs
                                        |> List.sum
                            in
                            \x -> toPct (Basics.abs x / total)
                        )
            }
        )


toPct : Float -> String
toPct x =
    String.fromInt (Basics.round (x * 100)) ++ "%"



-- Views


{-| -}
view :
    { x : Float
    , y : Float
    , ctx : W.Chart.Context msg x y z
    , stroke : Bool
    , color : String
    , label : String
    }
    -> SC.Svg msg
view props =
    S.g
        [ HA.attribute "style" ("--color:" ++ props.color)
        , if props.stroke then
            SA.class [ "w__m-stroke" ]

          else
            SA.class []
        ]
        [ S.text_
            [ SAP.x props.x
            , SAP.y props.y
            , SAP.fontSize props.ctx.fontSize.lg
            , SA.textAnchor ST.AnchorMiddle
            ]
            [ SC.text props.label
            ]
        ]


{-| -}
viewList :
    List Attribute
    ->
        { ctx : W.Chart.Context msg x y z
        , axisAttrs : W.Chart.Internal.RenderAxisYZ msg a
        , points : List ( W.Chart.RenderDatum, List W.Chart.RenderDatum )
        }
    -> SC.Svg msg
viewList =
    Attr.withAttrs defaultAttrs
        (\attrs_ props ->
            let
                step : Int
                step =
                    toStep props.ctx (List.length props.points)

                position : Position
                position =
                    if props.axisAttrs.isStacked then
                        Inside

                    else
                        attrs_.position

                attrs : Attributes
                attrs =
                    { attrs_ | position = position }
            in
            props.points
                |> List.indexedMap
                    (\index ( x, ys ) ->
                        let
                            visibleStep : Bool
                            visibleStep =
                                modBy step index == 0
                        in
                        if visibleStep then
                            let
                                formatPoint : W.Chart.RenderDatum -> String
                                formatPoint =
                                    attrs.format
                                        |> Maybe.map
                                            (\fn_ ->
                                                let
                                                    fn : Float -> String
                                                    fn =
                                                        fn_ (List.map .value ys)
                                                in
                                                \point -> fn point.value
                                            )
                                        |> Maybe.withDefault .valueString
                            in
                            viewStackPoint
                                { attrs = attrs
                                , ctx = props.ctx
                                , axisAttrs = props.axisAttrs
                                , x = x.valueScaled
                                , pointList = ys
                                }
                                :: (ys
                                        |> List.map
                                            (\y ->
                                                let
                                                    visibleHeight : Bool
                                                    visibleHeight =
                                                        abs (y.valueEnd - y.valueStart) >= props.ctx.fontSize.lg
                                                in
                                                if visibleHeight then
                                                    viewPoint
                                                        { attrs = attrs
                                                        , ctx = props.ctx
                                                        , x = x.valueScaled
                                                        , pointList = ys
                                                        , point = y
                                                        , format = formatPoint
                                                        }

                                                else
                                                    H.text ""
                                            )
                                   )

                        else
                            []
                    )
                |> List.concat
                |> S.g []
        )


{-| -}
viewBinsList :
    List Attribute
    ->
        { binScale : Scale.BandScale Int
        , ctx : W.Chart.Context msg x y z
        , points : List (W.Chart.PointXYZ x y z)
        }
    -> SC.Svg msg
viewBinsList =
    Attr.withAttrs defaultAttrs
        (\attrs props ->
            let
                binsCount : Int
                binsCount =
                    Scale.domain props.binScale
                        |> List.length

                totalBinsCount : Int
                totalBinsCount =
                    List.length props.points * binsCount

                step : Int
                step =
                    toStep props.ctx totalBinsCount
            in
            props.points
                |> List.indexedMap
                    (\index point ->
                        let
                            yCount : Int
                            yCount =
                                yBinCount props.ctx.y point.y
                        in
                        viewBinsListPoint attrs
                            { binScale = props.binScale
                            , binsCount = binsCount
                            , ctx = props.ctx
                            , axisAttrs = props.ctx.y
                            , x = point.x
                            , yz = point.y
                            , xIndex = index
                            , step = step
                            , offset = 0
                            }
                            ++ viewBinsListPoint attrs
                                { binScale = props.binScale
                                , binsCount = binsCount
                                , ctx = props.ctx
                                , axisAttrs = props.ctx.z
                                , x = point.x
                                , yz = point.z
                                , xIndex = index
                                , step = step
                                , offset = yCount
                                }
                    )
                |> List.concat
                |> S.g []
        )


viewBinsListPoint :
    Attributes
    ->
        { binScale : Scale.BandScale Int
        , binsCount : Int
        , ctx : W.Chart.Context msg x y z
        , axisAttrs : W.Chart.Internal.RenderAxisYZ msg a
        , x : W.Chart.Point x
        , yz : List (W.Chart.Point a)
        , xIndex : Int
        , step : Int
        , offset : Int
        }
    -> List (SC.Svg msg)
viewBinsListPoint attrs props =
    let
        yzPoints : List W.Chart.RenderDatum
        yzPoints =
            List.map .render props.yz

        xBase : Float
        xBase =
            props.x.render.valueStart
                + Scale.convert props.binScale props.offset

        xStart : Float
        xStart =
            if props.axisAttrs.isStacked then
                xBase + (Scale.bandwidth props.binScale * 0.5)

            else
                xBase

        formatPoint : W.Chart.RenderDatum -> String
        formatPoint =
            attrs.format
                |> Maybe.map
                    (\fn_ ->
                        let
                            fn : Float -> String
                            fn =
                                fn_ (List.map .value yzPoints)
                        in
                        \point -> fn point.value
                    )
                |> Maybe.withDefault .valueString

        position : Position
        position =
            if props.axisAttrs.isStacked then
                Center

            else
                attrs.position

        visibleStackStep : Bool
        visibleStackStep =
            modBy props.step (props.xIndex + props.offset) == 0

        stackPoint : SC.Svg msg
        stackPoint =
            if props.axisAttrs.isStacked && visibleStackStep then
                viewStackPoint
                    { attrs = attrs
                    , ctx = props.ctx
                    , axisAttrs = props.axisAttrs
                    , x = xStart
                    , pointList = List.map (\yz -> yz.render) props.yz
                    }

            else
                H.text ""
    in
    stackPoint
        :: (props.yz
                |> List.indexedMap
                    (\index point ->
                        let
                            step : Int
                            step =
                                (props.xIndex * props.binsCount) + (props.offset + index)

                            visibleHeight : Bool
                            visibleHeight =
                                abs (point.render.valueEnd - point.render.valueStart) >= props.ctx.fontSize.lg

                            visibleStep : Bool
                            visibleStep =
                                if props.axisAttrs.isStacked then
                                    visibleStackStep

                                else
                                    modBy props.step step == 0
                        in
                        if visibleHeight && visibleStep then
                            let
                                x : Float
                                x =
                                    if props.axisAttrs.isStacked then
                                        xStart

                                    else
                                        xStart + Scale.convert props.binScale index
                            in
                            viewPoint
                                { attrs = { attrs | position = position, stroke = position /= Outside }
                                , ctx = props.ctx
                                , x = x
                                , pointList = yzPoints
                                , point = point.render
                                , format = formatPoint
                                }

                        else
                            H.text ""
                    )
           )


labelMinWidth : Int
labelMinWidth =
    80


toStep : W.Chart.Context msg x y z -> Int -> Int
toStep ctx numPoints =
    (Basics.floor ctx.width // numPoints)
        |> (//) labelMinWidth
        |> toFloat
        |> round
        |> max 1


yBinCount : W.Chart.Internal.RenderAxisYZ msg y -> List (W.Chart.Point y) -> Int
yBinCount axis yList =
    case List.length yList of
        0 ->
            0

        length ->
            if axis.isStacked then
                1

            else
                length


viewStackPoint :
    { attrs : Attributes
    , ctx : W.Chart.Context msg x y z
    , axisAttrs : W.Chart.Internal.RenderAxisYZ msg a
    , x : Float
    , pointList : List W.Chart.RenderDatum
    }
    -> SC.Svg msg
viewStackPoint props =
    case ( props.axisAttrs.isStacked, props.axisAttrs.formatStack ) of
        ( True, Just format_ ) ->
            let
                attrs : Attributes
                attrs =
                    props.attrs

                yString : String
                yString =
                    format_ (List.map .value props.pointList)

                yStart : Float
                yStart =
                    List.minimum (List.map .valueStart props.pointList)
                        |> Maybe.withDefault 0.0

                yEnd : Float
                yEnd =
                    List.maximum (List.map .valueEnd props.pointList)
                        |> Maybe.withDefault 0.0

                yScaled : Float
                yScaled =
                    if yStart < props.axisAttrs.zero || yEnd == props.axisAttrs.zero then
                        yStart

                    else
                        yEnd

                yValue : Float
                yValue =
                    props.axisAttrs.zero - yScaled
            in
            viewPoint
                { attrs = { attrs | position = Outside }
                , ctx = props.ctx
                , x = props.x
                , pointList = props.pointList
                , point =
                    { color = W.Theme.Color.baseTextSubtle
                    , label = ""
                    , value = yValue
                    , valueString = yString
                    , valueScaled = yScaled
                    , valueStart = yStart
                    , valueEnd = yEnd
                    , isDefault = False
                    }
                , format = .valueString
                }

        _ ->
            H.text ""


viewPoint :
    { attrs : Attributes
    , ctx : W.Chart.Context msg x y z
    , x : Float
    , pointList : List W.Chart.RenderDatum
    , point : W.Chart.RenderDatum
    , format : W.Chart.RenderDatum -> String
    }
    -> SC.Svg msg
viewPoint props =
    if props.point.value == 0.0 then
        H.text ""

    else
        let
            y : Float
            y =
                case props.attrs.position of
                    Inside ->
                        if props.point.value >= 0.0 then
                            props.point.valueScaled + props.ctx.fontSize.lg * 1.3

                        else
                            props.point.valueScaled - props.ctx.fontSize.lg * 0.6

                    Outside ->
                        if props.point.value < 0.0 then
                            props.point.valueScaled + props.ctx.fontSize.lg * 1.3

                        else
                            props.point.valueScaled - props.ctx.fontSize.lg * 0.6

                    Center ->
                        (props.point.valueStart + ((props.point.valueEnd - props.point.valueStart) * 0.5)) + props.ctx.fontSize.lg * 0.4
        in
        view
            { ctx = props.ctx
            , x = props.x
            , y = y
            , stroke = props.attrs.stroke
            , color = props.point.color
            , label = props.format props.point
            }

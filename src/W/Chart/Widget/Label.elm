module W.Chart.Widget.Label exposing
    ( view, viewList, viewBinsList
    , format, formatAsPercentage, formatWithList, inside, centered
    , formatStack
    , Attribute
    )

{-|

@docs view, viewList, viewBinsList
@docs format, formatAsPercentage, formatWithList, inside, centered
@docs formatStack
@docs Attribute

-}

import Attr
import Html as H
import Scale
import Svg.Attributes
import Theme
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC
import TypedSvg.Types as ST
import W.Chart
import W.Chart.Internal



-- Attributes


{-| -}
type alias Attribute =
    Attr.Attr Attributes


type alias Attributes =
    { position : Position
    , format : Maybe (List Float -> Float -> String)
    , formatStack : Maybe (List Float -> String)
    }


defaultAttrs : Attributes
defaultAttrs =
    { position = Outside
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
    Attr.attr (\attr -> { attr | format = Just (\xs x -> toPct (x / List.sum xs)) })


toPct : Float -> String
toPct x =
    String.fromInt (Basics.round (x * 100)) ++ "%"



-- Views


{-| -}
view :
    { x : Float
    , y : Float
    , ctx : W.Chart.Context x y z
    , color : String
    , label : String
    }
    -> SC.Svg msg
view props =
    S.g
        []
        [ S.text_
            [ SAP.x props.x
            , SAP.y props.y
            , SAP.strokeWidth 6
            , Svg.Attributes.fill props.color
            , Svg.Attributes.stroke props.color
            , SAP.fontSize props.ctx.fontSize.lg
            , SA.textAnchor ST.AnchorMiddle
            ]
            [ SC.text props.label
            ]
        , S.text_
            [ SAP.x props.x
            , SAP.y props.y
            , SAP.strokeWidth 4
            , Svg.Attributes.fill Theme.baseForeground
            , Svg.Attributes.stroke Theme.baseBackground
            , Svg.Attributes.style "paint-order:stroke"
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
        { ctx : W.Chart.Context x y z
        , axisAttrs : W.Chart.Internal.RenderAxisYZ a
        , points : List ( W.Chart.RenderDatum, List W.Chart.RenderDatum )
        }
    -> SC.Svg msg
viewList =
    Attr.withAttrs defaultAttrs
        (\attrs_ props ->
            let
                xMaxLabels : Int
                xMaxLabels =
                    Basics.floor props.ctx.width // 60

                length : Int
                length =
                    List.length props.points

                step : Int
                step =
                    if length <= xMaxLabels then
                        1

                    else
                        length // xMaxLabels

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
        , ctx : W.Chart.Context x y z
        , points : List (W.Chart.PointXYZ x y z)
        }
    -> SC.Svg msg
viewBinsList =
    Attr.withAttrs defaultAttrs
        (\attrs props ->
            props.points
                |> List.concatMap
                    (\point ->
                        let
                            yCount : Int
                            yCount =
                                yBinCount props.ctx.y point.y
                        in
                        viewBinsListPoint attrs
                            { binScale = props.binScale
                            , ctx = props.ctx
                            , axisAttrs = props.ctx.y
                            , x = point.x
                            , yz = point.y
                            , offset = 0
                            }
                            ++ viewBinsListPoint attrs
                                { binScale = props.binScale
                                , ctx = props.ctx
                                , axisAttrs = props.ctx.z
                                , x = point.x
                                , yz = point.z
                                , offset = yCount
                                }
                    )
                |> S.g []
        )


viewBinsListPoint :
    Attributes
    ->
        { binScale : Scale.BandScale Int
        , ctx : W.Chart.Context x y z
        , axisAttrs : W.Chart.Internal.RenderAxisYZ a
        , x : W.Chart.Point x
        , yz : List (W.Chart.Point a)
        , offset : Int
        }
    -> List (SC.Svg msg)
viewBinsListPoint attrs props =
    let
        yzPoints : List W.Chart.RenderDatum
        yzPoints =
            List.map .render props.yz

        xStart : Float
        xStart =
            props.x.render.valueStart
                + (Scale.bandwidth props.binScale * 0.5)
                + Scale.convert props.binScale props.offset
    in
    viewStackPoint
        { attrs = attrs
        , ctx = props.ctx
        , axisAttrs = props.axisAttrs
        , x = xStart
        , pointList = List.map (\yz -> yz.render) props.yz
        }
        :: (props.yz
                |> List.indexedMap
                    (\index point ->
                        let
                            visibleHeight : Bool
                            visibleHeight =
                                abs (point.render.valueEnd - point.render.valueStart) >= props.ctx.fontSize.lg

                            position : Position
                            position =
                                if props.axisAttrs.isStacked then
                                    Center

                                else
                                    attrs.position
                        in
                        if visibleHeight then
                            let
                                x : Float
                                x =
                                    if props.axisAttrs.isStacked then
                                        xStart

                                    else
                                        xStart + Scale.convert props.binScale index
                            in
                            viewPoint
                                { attrs = { attrs | position = position }
                                , ctx = props.ctx
                                , x = x
                                , pointList = yzPoints
                                , point = point.render
                                }

                        else
                            H.text ""
                    )
           )


yBinCount : W.Chart.Internal.RenderAxisYZ y -> List (W.Chart.Point y) -> Int
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
    , ctx : W.Chart.Context x y z
    , axisAttrs : W.Chart.Internal.RenderAxisYZ a
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
                    { color = "#fff"
                    , label = ""
                    , value = yValue
                    , valueString = yString
                    , valueScaled = yScaled
                    , valueStart = yStart
                    , valueEnd = yEnd
                    , isDefault = False
                    }
                }

        _ ->
            H.text ""


viewPoint :
    { attrs : Attributes
    , ctx : W.Chart.Context x y z
    , x : Float
    , pointList : List W.Chart.RenderDatum
    , point : W.Chart.RenderDatum
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
            , color = props.point.color
            , label =
                props.attrs.format
                    |> Maybe.map (\fn -> fn (List.map .value props.pointList) props.point.value)
                    |> Maybe.withDefault props.point.valueString
            }

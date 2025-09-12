module W.Chart.Line exposing
    ( fromY, fromZ
    , smooth, dashed, areaAlways, lineAlways
    , showLabels, labelFormat, labelFormatWithList, labelsAsPercentages
    , onClick, onMouseEnter, onMouseLeave
    , Attribute
    )

{-|

@docs fromY, fromZ

@docs smooth, dashed, areaAlways, lineAlways
@docs showLabels, labelFormat, labelFormatWithList, labelsAsPercentages
@docs onClick, onMouseEnter, onMouseLeave
@docs Attribute

-}

import Attr
import Dict
import Html as H
import Path
import Shape
import SubPath
import Svg.Attributes
import Svg.Events as SE
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC
import TypedSvg.Types as ST
import W.Chart
import W.Chart.Internal
import W.Chart.Widget
import W.Chart.Widget.Label
import W.Svg.Circle


{-| -}
fromY : List (Attribute y msg) -> W.Chart.WidgetXY msg x y z a
fromY =
    Attr.withAttrs defaultAttrs
        (\attrs ->
            W.Chart.Widget.fromY (\ctx -> viewLines attrs ctx.y ctx.points.y)
                |> W.Chart.Widget.withLabels
                    (\ctx ->
                        if attrs.labels then
                            S.g
                                []
                                [ W.Chart.Widget.Label.viewList
                                    [ attrs.labelFormat ]
                                    { ctx = ctx
                                    , axisAttrs = ctx.y
                                    , points =
                                        ctx.points.byX
                                            |> Dict.values
                                            |> List.map
                                                (\data ->
                                                    ( data.x.render, data.yRender )
                                                )
                                    }
                                ]

                        else
                            H.text ""
                    )
                |> W.Chart.Widget.withHover (\_ _ point -> viewHover point.x point.y)
        )


{-| -}
fromZ : List (Attribute z msg) -> W.Chart.WidgetXYZ msg x y z a
fromZ =
    Attr.withAttrs defaultAttrs
        (\attrs ->
            W.Chart.Widget.fromZ (\ctx -> viewLines attrs ctx.z ctx.points.z)
                |> W.Chart.Widget.withLabels
                    (\ctx ->
                        if attrs.labels then
                            W.Chart.Widget.Label.viewList
                                [ attrs.labelFormat ]
                                { ctx = ctx
                                , axisAttrs = ctx.z
                                , points =
                                    ctx.points.byX
                                        |> Dict.values
                                        |> List.map
                                            (\data ->
                                                ( data.x.render, data.zRender )
                                            )
                                }

                        else
                            H.text ""
                    )
                |> W.Chart.Widget.withHover (\_ _ point -> viewHover point.x point.z)
        )



-- Attributes


{-| -}
type alias Attribute a msg =
    Attr.Attr (Attributes a msg)


type alias Attributes a msg =
    { smooth : Bool
    , dashed : Bool
    , area : Maybe Bool
    , labels : Bool
    , labelFormat : W.Chart.Widget.Label.Attribute
    , onClick : Maybe (a -> msg)
    , onMouseEnter : Maybe (a -> msg)
    , onMouseLeave : Maybe (a -> msg)
    }


defaultAttrs : Attributes a msg
defaultAttrs =
    { smooth = False
    , dashed = False
    , area = Nothing
    , labels = False
    , labelFormat = Attr.none
    , onClick = Nothing
    , onMouseEnter = Nothing
    , onMouseLeave = Nothing
    }


{-| -}
smooth : Attribute a msg
smooth =
    Attr.attr (\attr -> { attr | smooth = True })


{-| -}
dashed : Attribute a msg
dashed =
    Attr.attr (\attr -> { attr | dashed = True })


{-| -}
lineAlways : Attribute a msg
lineAlways =
    Attr.attr (\attr -> { attr | area = Just False })


{-| -}
areaAlways : Attribute a msg
areaAlways =
    Attr.attr (\attr -> { attr | area = Just True })


{-| -}
showLabels : Attribute a msg
showLabels =
    Attr.attr (\attr -> { attr | labels = True })


{-| -}
labelFormat : (Float -> String) -> Attribute a msg
labelFormat fn =
    Attr.attr (\attr -> { attr | labelFormat = W.Chart.Widget.Label.format fn })


{-| -}
labelFormatWithList : (List Float -> Float -> String) -> Attribute a msg
labelFormatWithList fn =
    Attr.attr (\attr -> { attr | labelFormat = W.Chart.Widget.Label.formatWithList fn })


{-| -}
labelsAsPercentages : Attribute a msg
labelsAsPercentages =
    Attr.attr (\attr -> { attr | labelFormat = W.Chart.Widget.Label.formatAsPercentage })


{-| -}
onClick : (a -> msg) -> Attribute a msg
onClick fn =
    Attr.attr (\attr -> { attr | onClick = Just fn })


{-| -}
onMouseEnter : (a -> msg) -> Attribute a msg
onMouseEnter fn =
    Attr.attr (\attr -> { attr | onMouseEnter = Just fn })


{-| -}
onMouseLeave : (a -> msg) -> Attribute a msg
onMouseLeave fn =
    Attr.attr (\attr -> { attr | onMouseLeave = Just fn })



-- Helpers


viewHover : W.Chart.Internal.DataPoint x -> List (W.Chart.Internal.DataPoint a) -> SC.Svg msg
viewHover x ys =
    ys
        |> List.filterMap
            (\y ->
                if y.render.isDefault then
                    Nothing

                else
                    Just
                        (W.Svg.Circle.view
                            [ Svg.Attributes.fill y.render.color
                            , Svg.Attributes.stroke "white"
                            ]
                            { x = x.render.valueScaled
                            , y = y.render.valueScaled
                            , radius = 4.0
                            }
                        )
            )
        |> S.g []


viewLines : Attributes a msg -> W.Chart.Internal.RenderAxisYZ a -> List ( W.Chart.Internal.ChartDatum a, List ( W.Chart.Internal.DataPoint x, W.Chart.Internal.DataPoint a ) ) -> SC.Svg msg
viewLines attrs axis axisPoints =
    let
        viewLine_ : Attributes a msg -> Int -> ( W.Chart.Internal.ChartDatum a, List ( W.Chart.Internal.DataPoint x, W.Chart.Internal.DataPoint a ) ) -> SC.Svg msg
        viewLine_ =
            case attrs.area of
                Just True ->
                    viewLineWithArea

                Just False ->
                    viewLine

                Nothing ->
                    if axis.isStacked || (List.length axis.data == 1) then
                        viewLineWithArea

                    else
                        viewLine
    in
    S.g [] (List.indexedMap (viewLine_ attrs) axisPoints)


viewLineWithArea : Attributes a msg -> Int -> ( W.Chart.Internal.ChartDatum a, List ( W.Chart.Internal.DataPoint x, W.Chart.Internal.DataPoint a ) ) -> SC.Svg msg
viewLineWithArea attrs index ( chartDatum, points ) =
    let
        areaPoints : List (Maybe ( ( Float, Float ), ( Float, Float ) ))
        areaPoints =
            points
                |> List.map
                    (\( x, y ) ->
                        Just
                            ( ( x.render.valueScaled
                              , y.render.valueStart
                              )
                            , ( x.render.valueScaled
                              , y.render.valueEnd
                              )
                            )
                    )

        linePoints : List (Maybe ( Float, Float ))
        linePoints =
            points
                |> List.map
                    (\( x, y ) ->
                        Just
                            ( x.render.valueScaled
                            , y.render.valueScaled
                            )
                    )

        gradientId : String
        gradientId =
            chartDatum.color
                |> String.replace "." "_"
                |> String.replace "," "_"
                |> String.replace "(" "_"
                |> String.replace ")" "_"
                |> String.replace "#" "_"
                |> (++) "w__charts--g-"
    in
    S.g
        []
        [ S.defs
            []
            [ S.linearGradient
                [ SA.id gradientId, SAP.x1 0, SAP.x2 0, SAP.y1 0, SAP.y2 1 ]
                [ S.stop [ SA.stopColor chartDatum.color, SA.offset "0%", SA.stopOpacity (ST.Opacity 0.3) ] []
                , S.stop [ SA.stopColor chartDatum.color, SA.offset "50%", SA.stopOpacity (ST.Opacity 0.2) ] []
                , S.stop [ SA.stopColor chartDatum.color, SA.offset "100%", SA.stopOpacity (ST.Opacity 0.0) ] []
                ]
            ]
        , Path.element
            (Shape.area (shape attrs) areaPoints)
            [ Svg.Attributes.class "w__charts--animate-fade"
            , Svg.Attributes.style ("animation-delay:" ++ String.fromInt (index * 400))
            , Svg.Attributes.fill ("url(#" ++ gradientId ++ ")")
            , attrs.onClick
                |> Maybe.map (\fn -> SE.onClick (fn chartDatum.datum))
                |> Maybe.withDefault (Svg.Attributes.class "")
            , attrs.onMouseEnter
                |> Maybe.map (\fn -> SE.onMouseOver (fn chartDatum.datum))
                |> Maybe.withDefault (Svg.Attributes.class "")
            , attrs.onMouseLeave
                |> Maybe.map (\fn -> SE.onMouseOut (fn chartDatum.datum))
                |> Maybe.withDefault (Svg.Attributes.class "")
            ]
        , Path.element
            (Shape.line (shape attrs) linePoints)
            [ Svg.Attributes.class "w__charts--animate-h-clip"
            , Svg.Attributes.style ("animation-delay:" ++ String.fromInt (index * 400))
            , Svg.Attributes.fill "transparent"
            , Svg.Attributes.strokeWidth "2px"
            , Svg.Attributes.stroke chartDatum.color
            , if attrs.dashed then
                SA.strokeDasharray "4 4"

              else
                Svg.Attributes.class ""
            , attrs.onClick
                |> Maybe.map (\fn -> SE.onClick (fn chartDatum.datum))
                |> Maybe.withDefault (Svg.Attributes.class "")
            , attrs.onMouseEnter
                |> Maybe.map (\fn -> SE.onMouseOver (fn chartDatum.datum))
                |> Maybe.withDefault (Svg.Attributes.class "")
            , attrs.onMouseLeave
                |> Maybe.map (\fn -> SE.onMouseOut (fn chartDatum.datum))
                |> Maybe.withDefault (Svg.Attributes.class "")
            ]
        ]


viewLine : Attributes a msg -> Int -> ( W.Chart.Internal.ChartDatum a, List ( W.Chart.Internal.DataPoint x, W.Chart.Internal.DataPoint a ) ) -> SC.Svg msg
viewLine attrs index ( chartDatum, points ) =
    let
        linePoints : List (Maybe ( Float, Float ))
        linePoints =
            points
                |> List.map
                    (\( x, y ) ->
                        Just
                            ( x.render.valueScaled
                            , y.render.valueStart
                            )
                    )
    in
    Path.element
        (Shape.line (shape attrs) linePoints)
        [ Svg.Attributes.class "w__charts--animate-h-clip"
        , Svg.Attributes.style ("animation-delay:" ++ String.fromInt (index * 400))
        , Svg.Attributes.fill "transparent"
        , Svg.Attributes.strokeWidth "2px"
        , Svg.Attributes.stroke chartDatum.color
        , if attrs.dashed then
            SA.strokeDasharray "4 4"

          else
            Svg.Attributes.class ""
        ]


shape : Attributes a msg -> List ( Float, Float ) -> SubPath.SubPath
shape attrs =
    if attrs.smooth then
        Shape.monotoneInXCurve

    else
        Shape.linearCurve

module W.Chart exposing
    ( globalStyles, fromX, fromXY, fromXYZ, ConfigX, ConfigXY, ConfigXYZ, Config, ChartAttribute
    , xAxis, axis, axisList
    , stacked, distribution
    , axisLabel, axisLabelPadding
    , defaultValue, safety
    , ticks, format, formatStack, formatLegends
    , noAxisLine, noGridLines
    , AxisAttribute
    , noLabels, dontAutoHideLabels
    , topLegends, bottomLegends
    , legendsPadding, legendsPaddingLeft, legendsPaddingRight, legendsPaddingX
    , header, footer
    , annotationsPadding, annotationsPaddingX
    , annotationsTopPadding, annotationsBottomPadding, annotationsLeftPadding, annotationsRightPadding
    , id
    , width, ratio, background
    , padding, paddingX, paddingY, paddingTop, paddingBottom, paddingLeft, paddingRight, paddingCustom
    , fontSize
    , view, WidgetX, WidgetXY, WidgetXYZ, Widget
    , withActive, withHover, groupByXY, onClick, onMouseEnter, onMouseLeave, PointX, PointXY, PointXYZ
    , onMouseEnterChart, onMouseLeaveChart
    , Context, Coordinates, Point, RenderDatum, AxisDataPoints
    , debug
    )

{-|


# Setup

@docs globalStyles, fromX, fromXY, fromXYZ, ConfigX, ConfigXY, ConfigXYZ, Config, ChartAttribute


# Axis

@docs xAxis, axis, axisList


# Axis Attributes

@docs stacked, distribution
@docs axisLabel, axisLabelPadding
@docs defaultValue, safety
@docs ticks, format, formatStack, formatLegends
@docs noAxisLine, noGridLines
@docs AxisAttribute


# Annotations


## Labels

@docs noLabels, dontAutoHideLabels


## Legends

@docs topLegends, bottomLegends
@docs legendsPadding, legendsPaddingLeft, legendsPaddingRight, legendsPaddingX


## Header (caption) and footer

@docs header, footer


## Annotation Padding

@docs annotationsPadding, annotationsPaddingX
@docs annotationsTopPadding, annotationsBottomPadding, annotationsLeftPadding, annotationsRightPadding


# Testing

@docs id


# Styles


## Size

@docs width, ratio, background


## Padding

@docs padding, paddingX, paddingY, paddingTop, paddingBottom, paddingLeft, paddingRight, paddingCustom


## Colors


## Font Sizes

@docs fontSize


# Widgets

@docs view, WidgetX, WidgetXY, WidgetXYZ, Widget


# Interaction

@docs withActive, withHover, groupByXY, onClick, onMouseEnter, onMouseLeave, PointX, PointXY, PointXYZ


# Chart Interaction

@docs onMouseEnterChart, onMouseLeaveChart


# Helpful Types

@docs Context, Coordinates, Point, RenderDatum, AxisDataPoints


# Debugging

@docs debug

-}

import Attr
import Axis
import Dict
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Scale
import Svg
import Svg.Attributes
import Svg.Events
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC
import TypedSvg.Types as ST
import W.Chart.Internal exposing (RenderDataFull, RenderDataYZ)
import W.Chart.Internal.Tick
import W.Chart.Internal.Voronoi
import W.Svg.Attributes
import W.Theme
import W.Theme.Color
import W.Theme.Font


{-| -}
type alias Config msg x y z point =
    W.Chart.Internal.Config msg x y z point


{-| -}
type alias ConfigX msg x =
    W.Chart.Internal.Config msg x () () { x : Point x }


{-| -}
type alias ConfigXY msg x y =
    W.Chart.Internal.Config msg x y () { x : Point x, y : List (Point y) }


{-| -}
type alias ConfigXYZ msg x y z =
    W.Chart.Internal.Config msg x y z { x : Point x, y : List (Point y), z : List (Point z) }


{-| -}
type alias PointX x =
    { x : Point x
    }


{-| -}
type alias PointXY x y =
    { x : Point x
    , y : List (Point y)
    }


{-| -}
type alias PointXYZ x y z =
    { x : Point x
    , y : List (Point y)
    , z : List (Point z)
    }


{-| -}
type alias Point a =
    W.Chart.Internal.DataPoint a


{-| -}
type alias Coordinates =
    { x : Float, y : Maybe Float }


{-| -}
type alias Widget msg x y z point =
    W.Chart.Internal.Widget msg x y z point


{-| -}
type alias WidgetX msg x y z a =
    Widget msg x y z { a | x : Point x }


{-| -}
type alias WidgetXY msg x y z a =
    Widget msg x y z { a | x : Point x, y : List (Point y) }


{-| -}
type alias WidgetXYZ msg x y z a =
    Widget msg x y z { a | x : Point x, y : List (Point y), z : List (Point z) }


{-| -}
type alias ChartAttribute msg =
    Attr.Attr (W.Chart.Internal.Attributes msg)


{-| -}
type alias AxisConfigX msg x =
    AxisConfig msg x x { xAxis : Bool }


{-| -}
type alias AxisConfigYZ msg x y =
    AxisConfig msg x y {}


{-| -}
type AxisConfig msg x a constraint
    = AxisConfig
        (W.Chart.Internal.AxisAttributes msg)
        { data : List a
        , toLabel : a -> String
        , toColor : a -> String
        , toValue : a -> x -> Maybe Float
        }


{-| -}
type alias AxisAttribute msg =
    Attr.Attr (W.Chart.Internal.AxisAttributes msg)


{-| -}
type alias AxisDataPoints x a =
    W.Chart.Internal.AxisDataPoints x a


{-| -}
type alias HoverAttribute msg g =
    Attr.Attr (W.Chart.Internal.HoverAttrs msg g)


{-| -}
type alias Context msg x y z =
    W.Chart.Internal.Context msg x y z


{-| -}
type alias RenderDatum =
    W.Chart.Internal.RenderDatum



-- Config


{-| -}
fromX :
    List (ChartAttribute msg)
    -> { x : AxisConfigX msg x }
    -> ConfigX msg x
fromX =
    Attr.withAttrs W.Chart.Internal.defaultAttrs
        (\attrs props ->
            let
                (AxisConfig xAttrs xData) =
                    props.x
            in
            W.Chart.Internal.Config
                { attrs = { attrs | xAxis = xAttrs }
                , toPoint = \point -> { x = point.x }
                , activePoint = Nothing
                , hover = Nothing
                , xData = Just xData
                , yData = Nothing
                , zData = Nothing
                }
        )


{-| -}
fromXY :
    List (ChartAttribute msg)
    ->
        { x : AxisConfigX msg x
        , y : AxisConfigYZ msg x y
        }
    -> ConfigXY msg x y
fromXY =
    Attr.withAttrs W.Chart.Internal.defaultAttrs
        (\attrs props ->
            let
                (AxisConfig xAttrs xData) =
                    props.x

                (AxisConfig yAttrs yData) =
                    props.y
            in
            W.Chart.Internal.Config
                { attrs = { attrs | xAxis = xAttrs, yAxis = yAttrs }
                , toPoint =
                    \point ->
                        { x = point.x
                        , y = point.y
                        }
                , activePoint = Nothing
                , hover = Nothing
                , xData = Just xData
                , yData = Just yData
                , zData = Nothing
                }
        )


{-| -}
fromXYZ :
    List (ChartAttribute msg)
    ->
        { x : AxisConfigX msg x
        , y : AxisConfigYZ msg x y
        , z : AxisConfigYZ msg x z
        }
    -> ConfigXYZ msg x y z
fromXYZ =
    Attr.withAttrs W.Chart.Internal.defaultAttrs
        (\attrs props ->
            let
                (AxisConfig xAttrs xData) =
                    props.x

                (AxisConfig yAttrs yData) =
                    props.y

                (AxisConfig zAttrs zData) =
                    props.z
            in
            W.Chart.Internal.Config
                { attrs = { attrs | xAxis = xAttrs, yAxis = yAttrs, zAxis = zAttrs }
                , toPoint = \point -> { x = point.x, y = point.y, z = point.z }
                , activePoint = Nothing
                , hover = Nothing
                , xData = Just xData
                , yData = Just yData
                , zData = Just zData
                }
        )



-- Config : Attributes


{-| -}
width : Int -> ChartAttribute msg
width v =
    Attr.attr (\a -> { a | width = toFloat v })


{-| -}
ratio : Float -> ChartAttribute msg
ratio v =
    Attr.attr (\a -> { a | ratio = v })


{-| -}
padding : Int -> ChartAttribute msg
padding v =
    withPadding
        (\_ ->
            { top = toFloat v
            , left = toFloat v
            , right = toFloat v
            , bottom = toFloat v
            }
        )


{-| -}
paddingX : Int -> ChartAttribute msg
paddingX v =
    withPadding (\p -> { p | left = toFloat v, right = toFloat v })


{-| -}
paddingY : Int -> ChartAttribute msg
paddingY v =
    withPadding (\p -> { p | top = toFloat v, bottom = toFloat v })


{-| -}
paddingTop : Int -> ChartAttribute msg
paddingTop v =
    withPadding (\p -> { p | top = toFloat v })


{-| -}
paddingBottom : Int -> ChartAttribute msg
paddingBottom v =
    withPadding (\p -> { p | bottom = toFloat v })


{-| -}
paddingLeft : Int -> ChartAttribute msg
paddingLeft v =
    withPadding (\p -> { p | left = toFloat v })


{-| -}
paddingRight : Int -> ChartAttribute msg
paddingRight v =
    withPadding (\p -> { p | right = toFloat v })


withPadding : (W.Chart.Internal.Padding -> W.Chart.Internal.Padding) -> ChartAttribute msg
withPadding fn =
    Attr.attr (\a -> { a | padding = fn a.padding })


{-| -}
paddingCustom :
    { top : Int
    , left : Int
    , right : Int
    , bottom : Int
    }
    -> ChartAttribute msg
paddingCustom v =
    Attr.attr
        (\a ->
            { a
                | padding =
                    { top = toFloat v.top
                    , left = toFloat v.left
                    , right = toFloat v.right
                    , bottom = toFloat v.bottom
                    }
            }
        )


{-| -}
id : String -> ChartAttribute msg
id v =
    Attr.attr (\a -> { a | id = Just v })


{-| -}
background : String -> ChartAttribute msg
background v =
    Attr.attr (\a -> { a | background = v })


{-| -}
fontSize : { small : Float, medium : Float, large : Float } -> ChartAttribute msg
fontSize v =
    Attr.attr (\a -> { a | fontSize = v })


{-| -}
debug : ChartAttribute msg
debug =
    Attr.attr (\a -> { a | debug = True })



-- Annotations


{-| This content will appear inside the `figcaption` for the chart `figure` element.
-}
header : List (H.Html msg) -> ChartAttribute msg
header v =
    Attr.attr (\a -> { a | header = Just v })


{-| This content will appear below all content, while respecting the annotations padding.
-}
footer : List (H.Html msg) -> ChartAttribute msg
footer v =
    Attr.attr (\a -> { a | footer = Just v })


{-| Displays the legends below the header (if present) and above the chart.
-}
topLegends : ChartAttribute msg
topLegends =
    Attr.attr (\a -> { a | legendDisplay = W.Chart.Internal.TopLegends })


{-| Displays the legends above the footer (if present) and below the chart.
-}
bottomLegends : ChartAttribute msg
bottomLegends =
    Attr.attr (\a -> { a | legendDisplay = W.Chart.Internal.BottomLegends })


{-| -}
annotationsPadding : Float -> ChartAttribute msg
annotationsPadding v =
    withAnnotationsPadding (\a -> { a | top = Just v, bottom = Just v })


{-| -}
annotationsPaddingX : Float -> ChartAttribute msg
annotationsPaddingX v =
    withAnnotationsPadding (\a -> { a | left = Just v, right = Just v })


{-| -}
annotationsTopPadding : Float -> ChartAttribute msg
annotationsTopPadding v =
    withAnnotationsPadding (\a -> { a | top = Just v })


{-| -}
annotationsBottomPadding : Float -> ChartAttribute msg
annotationsBottomPadding v =
    withAnnotationsPadding (\a -> { a | bottom = Just v })


{-| -}
annotationsLeftPadding : Float -> ChartAttribute msg
annotationsLeftPadding v =
    withAnnotationsPadding (\a -> { a | left = Just v })


{-| -}
annotationsRightPadding : Float -> ChartAttribute msg
annotationsRightPadding v =
    withAnnotationsPadding (\a -> { a | right = Just v })


withAnnotationsPadding : (W.Chart.Internal.PaddingOverride -> W.Chart.Internal.PaddingOverride) -> ChartAttribute msg
withAnnotationsPadding fn =
    Attr.attr (\a -> { a | annotationsPadding = fn a.annotationsPadding })


{-| Defines the space between legends and the header or footer.
-}
legendsPadding : Float -> ChartAttribute msg
legendsPadding v =
    withLegendsPadding (\a -> { a | top = Just v, bottom = Just v })


{-| -}
legendsPaddingX : Float -> ChartAttribute msg
legendsPaddingX v =
    withLegendsPadding (\a -> { a | left = Just v, right = Just v })


{-| -}
legendsPaddingLeft : Float -> ChartAttribute msg
legendsPaddingLeft v =
    withLegendsPadding (\a -> { a | left = Just v })


{-| -}
legendsPaddingRight : Float -> ChartAttribute msg
legendsPaddingRight v =
    withLegendsPadding (\a -> { a | right = Just v })


withLegendsPadding : (W.Chart.Internal.PaddingOverride -> W.Chart.Internal.PaddingOverride) -> ChartAttribute msg
withLegendsPadding fn =
    Attr.attr (\a -> { a | legendsPadding = fn a.legendsPadding })



-- Labels


{-| -}
noLabels : ChartAttribute msg
noLabels =
    Attr.attr (\attrs -> { attrs | labels = False })


{-| -}
dontAutoHideLabels : ChartAttribute msg
dontAutoHideLabels =
    Attr.attr (\attrs -> { attrs | labelsAutoHide = False })



-- Interactivity


{-| -}
onMouseEnterChart : msg -> ChartAttribute msg
onMouseEnterChart fn =
    Attr.attr (\a -> { a | onMouseEnter = Just fn })


{-| -}
onMouseLeaveChart : msg -> ChartAttribute msg
onMouseLeaveChart fn =
    Attr.attr (\a -> { a | onMouseLeave = Just fn })


{-| -}
onClick : ({ x : Float, y : Maybe Float } -> point -> msg) -> HoverAttribute msg point
onClick fn =
    Attr.attr (\a -> { a | onClick = Just fn })


{-| -}
onMouseEnter : ({ x : Float, y : Maybe Float } -> point -> msg) -> HoverAttribute msg point
onMouseEnter fn =
    Attr.attr (\a -> { a | onMouseEnter = Just fn })


{-| -}
onMouseLeave : ({ x : Float, y : Maybe Float } -> point -> msg) -> HoverAttribute msg point
onMouseLeave fn =
    Attr.attr (\a -> { a | onMouseLeave = Just fn })



-- Tooltips


{-| -}
withActive : Maybe Coordinates -> Config msg x y z point -> Config msg x y z point
withActive v (W.Chart.Internal.Config cfg) =
    W.Chart.Internal.Config { cfg | activePoint = v }


{-| -}
withHover : List (HoverAttribute msg point) -> Config msg x y z point -> Config msg x y z point
withHover =
    Attr.withAttrs W.Chart.Internal.defaultHoverAttrs
        (\tooltipAttrs (W.Chart.Internal.Config cfg) ->
            W.Chart.Internal.Config { cfg | hover = Just tooltipAttrs }
        )


{-| -}
groupByXY : HoverAttribute msg point
groupByXY =
    Attr.attr (\a -> { a | grouping = W.Chart.Internal.GroupByXY })



-- Axis


{-| -}
xAxis :
    List (AxisAttribute msg)
    ->
        { data : List x
        , toLabel : x -> String
        }
    -> AxisConfigX msg x
xAxis =
    Attr.withAttrs W.Chart.Internal.defaultAxisAttributes
        (\attrs props ->
            AxisConfig
                attrs
                { data = props.data
                , toLabel = props.toLabel
                , toColor = \_ -> ""
                , toValue = \_ _ -> Nothing
                }
        )


{-| -}
axis :
    List (AxisAttribute msg)
    ->
        { label : String
        , color : String
        , toValue : x -> Maybe Float
        }
    -> AxisConfigYZ msg x String
axis =
    Attr.withAttrs W.Chart.Internal.defaultAxisAttributes
        (\attrs props ->
            AxisConfig
                attrs
                { data = [ props.label ]
                , toLabel = \_ -> props.label
                , toColor = \_ -> props.color
                , toValue = \_ -> props.toValue
                }
        )


{-| -}
axisList :
    List (AxisAttribute msg)
    ->
        { data : List a
        , toLabel : a -> String
        , toColor : a -> String
        , toValue : a -> x -> Maybe Float
        }
    -> AxisConfigYZ msg x a
axisList =
    Attr.withAttrs W.Chart.Internal.defaultAxisAttributes
        (\attrs props -> AxisConfig attrs props)



-- Axis Attributes


{-| -}
axisLabel : String -> AxisAttribute msg
axisLabel v =
    Attr.attr (\attrs -> { attrs | label = Just v })


{-| -}
axisLabelPadding : Float -> AxisAttribute msg
axisLabelPadding v =
    Attr.attr (\attrs -> { attrs | labelPadding = Just v })


{-| -}
defaultValue : Float -> AxisAttribute msg
defaultValue v =
    Attr.attr (\attrs -> { attrs | defaultValue = v })


{-| -}
format : (Float -> String) -> AxisAttribute msg
format v =
    Attr.attr (\attrs -> { attrs | format = v })


{-| -}
formatStack : (List Float -> String) -> AxisAttribute msg
formatStack v =
    Attr.attr (\attrs -> { attrs | formatStack = Just v })


{-| -}
formatLegends : ({ label : String, color : String, values : List Float } -> List (H.Html msg)) -> AxisAttribute msg
formatLegends v =
    Attr.attr (\attrs -> { attrs | legendsFormat = Just v })


{-| -}
safety : Float -> AxisAttribute msg
safety v =
    Attr.attr (\attrs -> { attrs | safety = v })


{-| -}
ticks : Int -> AxisAttribute msg
ticks v =
    Attr.attr (\attrs -> { attrs | ticks = v })


{-| -}
stacked : AxisAttribute msg
stacked =
    Attr.attr (\attrs -> { attrs | stackType = W.Chart.Internal.Stacked })


{-| -}
distribution : AxisAttribute msg
distribution =
    Attr.attr (\attrs -> { attrs | stackType = W.Chart.Internal.Distribution })



-- TODO: Log scales doesn't work yet. Need to investigate it more.
-- logarithmic : Float -> AxisAttribute
-- logarithmic basis =
--     Attr.attr (\attrs -> { attrs | scale = W.Chart.Internal.Logarithmic basis })


{-| -}
noAxisLine : AxisAttribute msg
noAxisLine =
    Attr.attr (\attrs -> { attrs | showAxis = False })


{-| -}
noGridLines : AxisAttribute msg
noGridLines =
    Attr.attr (\attrs -> { attrs | showGrid = False })



-- View


{-| -}
view : List (W.Chart.Internal.Widget msg x y z point) -> Config msg x y z point -> H.Html msg
view widgets (W.Chart.Internal.Config cfg) =
    cfg.xData
        |> Maybe.map
            (\xData ->
                let
                    renderData : W.Chart.Internal.RenderData msg x y z
                    renderData =
                        W.Chart.Internal.toRenderData cfg xData

                    (W.Chart.Internal.RenderData d) =
                        renderData
                in
                H.figure
                    [ cfg.attrs.id
                        |> Maybe.map HA.id
                        |> Maybe.withDefault (HA.class "")
                    , HA.class "w__charts"
                    , HA.classList
                        [ ( "m--unfocus", True || cfg.hover /= Nothing )
                        , ( "m--debug", d.attrs.debug )
                        , ( "m--labels-autohide", d.attrs.labelsAutoHide )
                        ]
                    , W.Theme.styleList
                        [ ( "--w-charts-font-sm", W.Chart.Internal.toPx d.ctx.fontSize.sm )
                        , ( "--w-charts-font-md", W.Chart.Internal.toPx d.ctx.fontSize.md )
                        , ( "--w-charts-font-lg", W.Chart.Internal.toPx d.ctx.fontSize.lg )
                        ]
                    ]
                    [ viewTopAnnotations d
                    , H.div
                        [ W.Theme.styleList
                            [ ( "padding-top", W.Chart.Internal.toPx d.spacings.padding.top )
                            , ( "padding-bottom", W.Chart.Internal.toPx d.spacings.padding.bottom )
                            , ( "padding-left", W.Chart.Internal.toPx d.spacings.padding.left )
                            , ( "padding-right", W.Chart.Internal.toPx d.spacings.padding.right )
                            ]
                        , W.Chart.Internal.maybeAttr cfg.attrs.onMouseEnter HE.onMouseEnter
                        , W.Chart.Internal.maybeAttr cfg.attrs.onMouseLeave HE.onMouseLeave
                        ]
                        [ Svg.svg
                            [ SA.viewBox 0 0 d.spacings.chart.width d.spacings.chart.height
                            , SA.class [ "w__charts--svg" ]
                            ]
                            [ S.g
                                [ SA.class [ "w__charts--axis" ] ]
                                [ -- Grid
                                  viewYGrid renderData
                                , viewXGrid d.ctx

                                -- Labels
                                , viewAxisLabels renderData

                                -- Axis
                                , viewYAxis renderData
                                , viewZAxis renderData
                                , viewXAxis d.ctx
                                ]

                            -- Elements & Hover
                            , viewWidgets "bg" .background renderData widgets
                            , viewWidgets "main" .main renderData widgets
                            , viewWidgets "fg" .foreground renderData widgets
                            , if d.attrs.labels then
                                viewWidgets "labels" .labels renderData widgets

                              else
                                H.text ""
                            , viewActive cfg d.ctx widgets
                            , viewHoverAndLabels cfg d.ctx widgets
                            ]
                        ]
                    , viewBottomAnnotations d
                    ]
            )
        |> Maybe.withDefault (H.text "")


viewTopAnnotations : RenderDataFull msg x y z -> H.Html msg
viewTopAnnotations renderData =
    let
        paddingLeft_ : String
        paddingLeft_ =
            renderData.attrs.annotationsPadding.left
                |> Maybe.withDefault renderData.spacings.padding.left
                |> W.Chart.Internal.toPx

        paddingRight_ : String
        paddingRight_ =
            renderData.attrs.annotationsPadding.right
                |> Maybe.withDefault renderData.spacings.padding.right
                |> W.Chart.Internal.toPx

        paddingTop_ : String
        paddingTop_ =
            renderData.attrs.annotationsPadding.top
                |> Maybe.withDefault renderData.spacings.padding.top
                |> W.Chart.Internal.toPx
    in
    case ( renderData.attrs.legendDisplay == W.Chart.Internal.TopLegends, renderData.attrs.header ) of
        ( False, Nothing ) ->
            H.text ""

        ( _, _ ) ->
            H.div
                [ HA.class "w__charts__annotations"
                , HA.style "padding" (paddingTop_ ++ " " ++ paddingRight_ ++ " 0 " ++ paddingLeft_)
                ]
                [ case renderData.attrs.header of
                    Just header_ ->
                        H.figcaption
                            [ renderData.attrs.legendsPadding.bottom
                                |> W.Chart.Internal.maybeFilter (\_ -> renderData.attrs.legendDisplay == W.Chart.Internal.TopLegends)
                                |> Maybe.map
                                    (\legendPadding_ ->
                                        HA.style "padding-bottom" (W.Chart.Internal.toPx legendPadding_)
                                    )
                                |> Maybe.withDefault (HA.class "")
                            ]
                            header_

                    Nothing ->
                        H.text ""
                , if renderData.attrs.legendDisplay == W.Chart.Internal.TopLegends then
                    viewLegends renderData

                  else
                    H.text ""
                ]


viewBottomAnnotations : RenderDataFull msg x y z -> H.Html msg
viewBottomAnnotations renderData =
    let
        paddingLeft_ : String
        paddingLeft_ =
            renderData.attrs.annotationsPadding.left
                |> Maybe.withDefault renderData.spacings.padding.left
                |> W.Chart.Internal.toPx

        paddingRight_ : String
        paddingRight_ =
            renderData.attrs.annotationsPadding.right
                |> Maybe.withDefault renderData.spacings.padding.right
                |> W.Chart.Internal.toPx

        paddingBottom_ : String
        paddingBottom_ =
            renderData.attrs.annotationsPadding.bottom
                |> Maybe.withDefault renderData.spacings.padding.bottom
                |> W.Chart.Internal.toPx
    in
    case ( renderData.attrs.legendDisplay == W.Chart.Internal.BottomLegends, renderData.attrs.footer ) of
        ( False, Nothing ) ->
            H.text ""

        ( _, _ ) ->
            H.div
                [ HA.class "w__charts__annotations"
                , HA.style "padding" ("0 " ++ paddingRight_ ++ " " ++ paddingBottom_ ++ " " ++ paddingLeft_)
                ]
                [ if renderData.attrs.legendDisplay == W.Chart.Internal.BottomLegends then
                    viewLegends renderData

                  else
                    H.text ""
                , case renderData.attrs.footer of
                    Just footer_ ->
                        H.footer
                            [ renderData.attrs.legendsPadding.top
                                |> W.Chart.Internal.maybeFilter
                                    (\_ ->
                                        renderData.attrs.legendDisplay == W.Chart.Internal.BottomLegends
                                    )
                                |> Maybe.map
                                    (\legendPadding_ ->
                                        HA.style "padding-top" (W.Chart.Internal.toPx legendPadding_)
                                    )
                                |> Maybe.withDefault (HA.class "")
                            ]
                            footer_

                    Nothing ->
                        H.text ""
                ]


viewLegends : RenderDataFull msg x y z -> H.Html msg
viewLegends renderData =
    H.div
        [ HA.class "w__charts__legends"
        , (\x -> HA.style "padding-left" (W.Chart.Internal.toPx x))
            |> W.Chart.Internal.maybeAttr renderData.attrs.legendsPadding.left
        , (\x -> HA.style "padding-right" (W.Chart.Internal.toPx x))
            |> W.Chart.Internal.maybeAttr renderData.attrs.legendsPadding.right
        ]
        [ renderData.y
            |> Maybe.map (viewAxisLegends renderData renderData.ctx.y)
            |> Maybe.withDefault (H.text "")
        , renderData.z
            |> Maybe.map (viewAxisLegends renderData renderData.ctx.z)
            |> Maybe.withDefault (H.text "")
        ]


viewAxisLegends : RenderDataFull msg x y z -> W.Chart.Internal.RenderAxisYZ msg a -> RenderDataYZ x a -> H.Html msg
viewAxisLegends chartAttrs axisAttrs axisData =
    let
        toLabel :
            { datum : W.Chart.Internal.ChartDatum a
            , values : List (W.Chart.Internal.DataPoint a)
            , stackedValues : List ( Float, Float )
            }
            -> List (H.Html msg)
        toLabel item =
            case axisAttrs.formatLegends of
                Just fn ->
                    fn
                        { label = axisData.toLabel item.datum.datum
                        , color = axisData.toColor item.datum.datum
                        , values = List.map (\x -> x.render.value) item.values
                        }

                Nothing ->
                    [ H.text (axisData.toLabel item.datum.datum) ]
    in
    H.section
        [ HA.class "w__charts__legends__axis" ]
        [ axisAttrs.label
            |> W.Chart.Internal.maybeFilter (\_ -> chartAttrs.attrs.showLegendAxisLabels)
            |> Maybe.map
                (\axisLabel_ ->
                    H.p
                        [ HA.class "w__charts__legends__title" ]
                        [ H.text axisLabel_ ]
                )
            |> Maybe.withDefault (H.text "")
        , H.div
            [ HA.class "w__charts__legends__list" ]
            (axisData.values
                |> List.map
                    (\item ->
                        H.p [ HA.class "w__charts__legends__item" ]
                            [ H.span
                                [ HA.style "background" (axisData.toColor item.datum.datum)
                                , HA.class "w__charts__legends__color"
                                ]
                                []
                            , H.span [] (toLabel item)
                            ]
                    )
            )
        ]



-- Static Elements


viewWidgets :
    String
    -> (W.Chart.Internal.WidgetData msg x y z point -> Maybe (Context msg x y z -> Svg.Svg msg))
    -> W.Chart.Internal.RenderData msg x y z
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> SC.Svg msg
viewWidgets class getter (W.Chart.Internal.RenderData { ctx }) widgets =
    widgets
        |> List.filterMap (\(W.Chart.Internal.Widget w) -> Maybe.map (\el_ -> el_ ctx) (getter w))
        |> S.g [ SA.class [ "w__charts--" ++ class ] ]



-- Hover Elements


viewActive :
    W.Chart.Internal.ConfigData msg x y z point
    -> W.Chart.Internal.Context msg x y z
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> SC.Svg msg
viewActive config ctx widgets =
    config.activePoint
        |> Maybe.andThen (\coords -> W.Chart.Internal.dataFromCoords coords ctx)
        |> Maybe.map
            (\point ->
                S.g [ Svg.Attributes.class "w__charts--active" ]
                    [ viewHoverWidgets ctx point (config.toPoint point) widgets ]
            )
        |> Maybe.withDefault (H.text "")


viewHoverAndLabels :
    W.Chart.Internal.ConfigData msg x y z point
    -> W.Chart.Internal.Context msg x y z
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> SC.Svg msg
viewHoverAndLabels config ctx widgets =
    case config.hover of
        Just hoverAttrs ->
            case hoverAttrs.grouping of
                W.Chart.Internal.GroupByX ->
                    viewHoverX config hoverAttrs ctx widgets

                W.Chart.Internal.GroupByXY ->
                    viewHoverNearest config hoverAttrs ctx widgets

        Nothing ->
            H.text ""


viewHoverX :
    W.Chart.Internal.ConfigData msg x y z point
    -> W.Chart.Internal.HoverAttrs msg point
    -> W.Chart.Internal.Context msg x y z
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> SC.Svg msg
viewHoverX config hoverAttrs ctx widgets =
    ctx.points.byX
        |> Dict.values
        |> List.filter
            (\pointData ->
                let
                    yCheck : Bool
                    yCheck =
                        List.any (not << .isDefault << .render) pointData.y

                    zCheck : Bool
                    zCheck =
                        List.any (not << .isDefault << .render) pointData.z
                in
                yCheck || zCheck
            )
        |> List.concatMap
            (\pointData ->
                let
                    point : point
                    point =
                        config.toPoint pointData
                in
                [ S.rect
                    ([ SAP.x pointData.x.render.valueStart
                     , SAP.y 0
                     , SAP.width (Scale.bandwidth ctx.x.binScale)
                     , SAP.height ctx.height
                     ]
                        ++ viewHoverAttrs hoverAttrs pointData point
                    )
                    []
                , viewHoverContent
                    ctx
                    pointData
                    point
                    widgets
                ]
            )
        |> S.g []


viewHoverNearest :
    W.Chart.Internal.ConfigData msg x y z point
    -> W.Chart.Internal.HoverAttrs msg point
    -> W.Chart.Internal.Context msg x y z
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> SC.Svg msg
viewHoverNearest config hoverAttrs ctx widgets =
    W.Chart.Internal.Voronoi.view
        (\pointData polygon ->
            let
                point : point
                point =
                    config.toPoint pointData
            in
            [ polygon (viewHoverAttrs hoverAttrs pointData point)
            , viewHoverContent ctx pointData point widgets
            ]
        )
        ctx


viewHoverContent :
    W.Chart.Internal.Context msg x y z
    -> W.Chart.Internal.ChartPoint x y z
    -> point
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> SC.Svg msg
viewHoverContent ctx pointData point widgets =
    S.g
        [ SA.class [ "w__charts--hover" ] ]
        [ viewChartPointCoords ctx pointData
        , viewHoverWidgets ctx pointData point widgets
        ]


viewChartPointCoords :
    W.Chart.Internal.Context msg x y z
    -> W.Chart.Internal.ChartPoint x y z
    -> SC.Svg msg
viewChartPointCoords ctx point =
    case ( ctx.isDebugging, point.pos.y ) of
        ( True, Just y ) ->
            S.circle
                [ SAP.cx point.pos.x
                , SAP.cy y
                , SAP.r 2.0
                , Svg.Attributes.fill "red"
                ]
                []

        _ ->
            H.text ""


viewHoverAttrs : W.Chart.Internal.HoverAttrs msg point -> W.Chart.Internal.ChartPoint x y z -> point -> List (Svg.Attribute msg)
viewHoverAttrs hoverAttrs pointData point =
    [ SA.class [ "w__charts--hover-target" ]
    , Svg.Attributes.fill "transparent"
    , W.Svg.Attributes.maybe hoverAttrs.onClick (\fn -> Svg.Events.onClick (fn pointData.pos point))
    , W.Svg.Attributes.maybe hoverAttrs.onMouseEnter (\fn -> Svg.Events.onMouseOver (fn pointData.pos point))
    , W.Svg.Attributes.maybe hoverAttrs.onMouseLeave (\fn -> Svg.Events.onMouseOut (fn pointData.pos point))
    ]


viewHoverWidgets :
    Context msg x y z
    -> W.Chart.Internal.ChartPoint x y z
    -> point
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> SC.Svg msg
viewHoverWidgets ctx pointData point widgets =
    widgets
        |> List.filterMap
            (\(W.Chart.Internal.Widget w) ->
                w.hover
                    |> Maybe.map (\fn -> fn ctx pointData.pos point)
            )
        |> W.Chart.Internal.maybeIf (not << List.isEmpty)
        |> Maybe.map (S.g [])
        |> Maybe.withDefault (H.text "")



-- Axis Labels


viewAxisLabels : W.Chart.Internal.RenderData msg x y z -> SC.Svg msg
viewAxisLabels (W.Chart.Internal.RenderData d) =
    S.g
        []
        [ d.attrs.yAxis.label
            |> W.Chart.Internal.maybeFilter (\_ -> W.Chart.Internal.isJust d.y)
            |> Maybe.map
                (\label ->
                    W.Chart.Internal.viewTranslate
                        { y = d.spacings.chart.height * 0.5
                        , x =
                            d.attrs.yAxis.labelPadding
                                |> Maybe.withDefault (d.ctx.fontSize.lg * 4.0)
                                |> (*) -1
                        }
                        [ S.text_
                            [ SA.transform [ ST.Rotate 270 0 0 ]
                            , SA.textAnchor ST.AnchorMiddle
                            , SAP.fontSize d.ctx.fontSize.lg
                            , SAP.x 0
                            , SAP.y 0
                            , Svg.Attributes.fill W.Theme.Color.textSubtle
                            ]
                            [ SC.text label ]
                        ]
                )
            |> Maybe.withDefault (H.text "")
        , d.attrs.zAxis.label
            |> W.Chart.Internal.maybeFilter (\_ -> W.Chart.Internal.isJust d.z)
            |> Maybe.map
                (\label ->
                    W.Chart.Internal.viewTranslate
                        { y = d.spacings.chart.height * 0.5
                        , x =
                            d.attrs.zAxis.labelPadding
                                |> Maybe.withDefault (d.spacings.chart.width + (d.ctx.fontSize.lg * 4.0))
                        }
                        [ S.text_
                            [ SA.transform [ ST.Rotate 90 0 0 ]
                            , SA.textAnchor ST.AnchorMiddle
                            , SAP.fontSize d.ctx.fontSize.lg
                            , SAP.x 0
                            , SAP.y 0
                            , Svg.Attributes.fill W.Theme.Color.textSubtle
                            ]
                            [ SC.text label ]
                        ]
                )
            |> Maybe.withDefault (H.text "")
        , d.attrs.xAxis.label
            |> Maybe.map
                (\label ->
                    S.text_
                        [ SA.textAnchor ST.AnchorMiddle
                        , SAP.fontSize d.ctx.fontSize.lg
                        , SAP.x (d.spacings.chart.width * 0.5)
                        , d.attrs.xAxis.labelPadding
                            |> Maybe.withDefault (d.ctx.fontSize.lg * 4.0)
                            |> (+) d.spacings.chart.height
                            |> SAP.y
                        , Svg.Attributes.fill W.Theme.Color.textSubtle
                        ]
                        [ SC.text label ]
                )
            |> Maybe.withDefault (H.text "")
        ]



--  Axes & Lines


viewYGrid : W.Chart.Internal.RenderData msg x y z -> SC.Svg msg
viewYGrid (W.Chart.Internal.RenderData d) =
    case ( d.attrs.yAxis.showGrid, d.y ) of
        ( True, Just yData ) ->
            Scale.ticks yData.scale d.attrs.yAxis.ticks
                |> List.map
                    (\tick ->
                        let
                            y : Float
                            y =
                                Scale.convert yData.scale tick
                        in
                        S.line
                            [ SA.x1 (ST.px 0)
                            , SA.x2 (ST.px d.spacings.chart.width)
                            , SA.y1 (ST.px y)
                            , SA.y2 (ST.px y)
                            , SA.strokeWidth (ST.px 1.0)
                            , Svg.Attributes.stroke W.Theme.Color.tint
                            ]
                            []
                    )
                |> S.g []

        _ ->
            H.text ""


viewXGrid : Context msg x y z -> SC.Svg msg
viewXGrid ctx =
    if ctx.x.showGrid then
        ctx.points.byX
            |> Dict.values
            |> W.Chart.Internal.Tick.toTicks ctx.x.ticks
            |> List.map
                (\{ x } ->
                    S.line
                        [ SAP.x1 x.render.valueScaled
                        , SAP.x2 x.render.valueScaled
                        , SAP.y1 0
                        , SAP.y2 ctx.height
                        , SA.strokeWidth (ST.px 1.0)
                        , Svg.Attributes.stroke W.Theme.Color.tint
                        ]
                        []
                )
            |> S.g []

    else
        H.text ""


viewXAxis : Context msg x y z -> SC.Svg msg
viewXAxis ctx =
    if ctx.x.showAxis then
        W.Chart.Internal.viewTranslate
            { x = 0
            , y = ctx.height
            }
            [ ctx.points.byX
                |> Dict.values
                |> W.Chart.Internal.Tick.toTicks ctx.x.ticks
                |> List.map
                    (\xData ->
                        S.text_
                            [ SA.textAnchor ST.AnchorMiddle
                            , SAP.y (ctx.fontSize.sm * 2)
                            , SAP.x xData.x.render.valueScaled
                            , Svg.Attributes.fill W.Theme.Color.textSubtle
                            , SAP.fontSize ctx.fontSize.sm
                            ]
                            [ SC.text xData.x.render.label ]
                    )
                |> S.g []
            ]

    else
        H.text ""


viewYAxis : W.Chart.Internal.RenderData msg x y z -> SC.Svg msg
viewYAxis (W.Chart.Internal.RenderData d) =
    case ( d.attrs.yAxis.showAxis, d.y ) of
        ( True, Just _ ) ->
            let
                axisFormat : Float -> String
                axisFormat =
                    if d.ctx.y.isDistribution then
                        W.Chart.Internal.formatPct

                    else
                        d.ctx.y.format
            in
            S.g []
                [ S.g
                    [ SA.class [ "w__charts--y-axis" ] ]
                    [ viewAxis
                        Axis.left
                        { ticks = d.ctx.y.ticks
                        , scale = d.ctx.y.scale
                        , format = axisFormat
                        }
                    ]
                , S.line
                    [ SA.x1 (ST.px 0)
                    , SA.x2 (ST.px d.spacings.chart.width)
                    , SA.y1 (ST.px d.ctx.y.zero)
                    , SA.y2 (ST.px d.ctx.y.zero)
                    , SA.strokeWidth (ST.px 1.0)
                    , Svg.Attributes.stroke W.Theme.Color.accent
                    ]
                    []
                ]

        _ ->
            H.text ""


viewZAxis : W.Chart.Internal.RenderData msg x y z -> SC.Svg msg
viewZAxis (W.Chart.Internal.RenderData d) =
    case ( d.attrs.zAxis.showAxis, d.z ) of
        ( True, Just _ ) ->
            let
                axisFormat : Float -> String
                axisFormat =
                    if d.ctx.z.isDistribution then
                        W.Chart.Internal.formatPct

                    else
                        d.ctx.z.format
            in
            W.Chart.Internal.viewTranslate
                { x = d.spacings.chart.width
                , y = 0
                }
                [ S.g
                    [ SA.class [ "w__charts--z-axis" ] ]
                    [ viewAxis
                        Axis.right
                        { ticks = d.ctx.z.ticks
                        , scale = d.ctx.z.scale
                        , format = axisFormat
                        }
                    ]
                ]

        _ ->
            H.text ""



-- View Helpers


viewAxis :
    (List (Axis.Attribute a) -> Axis.RenderableScale x domain range a -> SC.Svg msg)
    ->
        { ticks : Int
        , format : a -> String
        , scale : Axis.RenderableScale x domain range a
        }
    -> SC.Svg msg
viewAxis axis_ props =
    axis_
        [ Axis.tickCount props.ticks
        , Axis.tickSizeOuter 0
        , Axis.tickSizeInner 6
        , Axis.tickPadding 6
        , Axis.tickFormat props.format
        ]
        props.scale



-- Styles


{-| -}
globalStyles : SC.Svg msg
globalStyles =
    H.node "style"
        []
        [ H.text ("""
            /* Basics */

            .w__charts,
            .w__charts text {
                font-family: var(--theme-font-text), sans-serif;
            }

            .w__charts text {
                font-weight: 600;
            }

            /* Prevent Tooltip Clipping */

            .w__charts--svg,
            .w__charts--tooltip-wrapper {
                overflow: visible;
            }

            /* Unfocus */

            .w__charts.m--unfocus:hover .w__charts--main {
                filter: grayscale(0%);
            }

            /* Hover */

            .w__charts--active,
            .w__charts--hover {
                pointer-events: none;
            }
            .w__charts--hover {
                display: none;
            }
            .w__charts--hover-target:hover + .w__charts--hover {
                display: block;
            }

            /* Chart */

            .w__charts {
                padding: 0;
                margin: 0;
            }

            /* Labels */

            .w__charts--labels {
                animation: w__charts--fade 0.2s ease-out forwards;
            }

            .w__charts.m--labels-autohide:has(.w__charts--hover-target:hover) .w__charts--labels,
            .w__charts.m--labels-autohide:has(.w__charts--active) .w__charts--labels {
                display: none;
            }

            /* Debug */

            .w__charts.m--debug .w__charts--hover-target {
                fill: rgba(255, 0, 0, 0.05);
                stroke: rgba(255, 0, 0, 0.1);
            }
            .w__charts.m--debug .w__charts--hover-target:hover {
                fill: rgba(255, 0, 0, 0.05);
                stroke: rgba(255, 0, 0, 0.5);
            }

            /* Tooltip */

            .w__charts--tooltip {
                display: flex;
                align-items: flex-end;
                justify-content: flex-start;
                box-sizing: border-box;
                font-family: inherit;
                font-size: var(--w-charts-font-md);
                line-height: 1;
                padding: 8px 0;
            }
            .w__charts--tooltip.m--align-left {
                justify-content: flex-end;
            }
            .w__charts--tooltip.m--align-top {
                align-items: flex-start;
            }
            .w__charts--tooltip.m--align-center {
                align-items: center;
            }

            .w__charts--tooltip-x,
            .w__charts--tooltip-yz--label,
            .w__charts--tooltip-yz--list,
            .w__charts--tooltip-yz--item {
                margin: 0;
            }

            .w__charts--tooltip-yz--label {
                display: flex;
                align-items: center;
                justify-content: space-between;
            }

            .w__charts--tooltip-x {
                padding: 4px;
                font-weight: normal;
            }

            .w__charts--tooltip-x-label {
                color: """ ++ W.Theme.Color.baseTextSubtle ++ """;
            }

            .w__charts--tooltip-x,
            .w__charts--tooltip-yz--label {
                font-size: inherit;
            }

            .w__charts--tooltip-x,
            .w__charts--tooltip-yz--item {
                display: flex;
                align-items: center;
                justify-content: space-between;
                gap: 4px;
            }
            .w__charts--tooltip-x--label,
            .w__charts--tooltip-yz--item-label {
                flex-grow: 1;
            }

            .w__charts--tooltip-x--value,
            .w__charts--tooltip-yz--item-color,
            .w__charts--tooltip-yz--item-value {
                flex-shrink: 0;
            }

            .w__charts--tooltip-yz {
                border-top: 1px solid """ ++ W.Theme.Color.baseTintSubtle ++ """;
                padding: 0 4px;
            }

            .w__charts--tooltip-yz--label {
                padding: 4px 0 0;
            }

            .w__charts--tooltip-yz--list {
                list-style-type: none;
                margin: 0;
                padding: 4px 0;
            }

            .w__charts--tooltip-yz--item {
                padding: 2px 0;
                margin: 0;
            }
            .w__charts--tooltip-yz--item-color {
                height: 8px;
                width: 8px;
                border-radius: 2px;
            }
            .w__charts--tooltip-yz--item-label {
                padding: 0 8px 0 2px;
            }

            /* Axis & Labels */

            .w__charts--axis text {
                font-family: var(--theme-font-text), sans-serif;
                font-size: var(--w-charts-font-sm);
                letter-spacing: 0.05em;
            }

            .w__charts .tick text {
                fill: """ ++ W.Theme.Color.baseTextSubtle ++ """;
                font-family: """ ++ W.Theme.Font.text ++ """;
            }

            .w__charts--x-axis path.domain,
            .w__charts--y-axis path.domain,
            .w__charts--z-axis path.domain {
                stroke: """ ++ W.Theme.Color.accent ++ """;
                stroke-width: 1px;
            }

            .w__charts--x-axis .tick line,
            .w__charts--y-axis .tick line,
            .w__charts--z-axis .tick line {
                stroke: """ ++ W.Theme.Color.accent ++ """;
            }

            /* Labels */

            .w__charts--labels {
                opacity: 0.0;
                animation: w__charts--fade 0.4s ease-out forwards;
                animation-delay: 0.2s;
            }

            .w__charts--labels text {
                fill: color-mix(in srgb, var(--color), """ ++ W.Theme.Color.baseText ++ """ 60%);
                stroke: """ ++ W.Theme.Color.baseBg ++ """;
                stroke-width: 2px;
                stroke-linejoin: bevel;
                font-weight: 600;
                paint-order: stroke;
                font-family: var(--theme-font-text), sans-serif;
                font-size: var(--w-charts-font-sm);
                letter-spacing: 0.05em;
            }

            .w__charts--labels .w__m-stroke text {
                fill: """ ++ W.Theme.Color.baseBg ++ """;
                stroke: color-mix(in srgb, var(--color), """ ++ W.Theme.Color.baseText ++ """ 60%);
                stroke-width: 3px;
            }

            /* Annotations */

            .w__charts__annotations {}

            /* Legends */

            .w__charts__legends {
                display: flex;
                gap: 3em;
                color: rgb(var(--w-text));
                font-size: var(--w-charts-font-md);
            }

            .w__charts__legends__axis {
                display: flex;
                flex-direction: column;
                gap: 0.75em;
            }

            .w__charts__legends__title {
                margin: 0;
                color: rgb(var(--w-text-subtle));
                font-size: var(--w-charts-font-lg);
            }

            .w__charts__legends__list {
                display: flex;
                flex-wrap: wrap;
                gap: 1.75em;
            }

            .w__charts__legends__item {
                margin: 0;
                display: flex;
                align-items: center;
                gap: 8px;
                line-height: 1;
            }

            .w__charts__legends__color {
                display: block;
                width: 0.75em;
                height: 0.75em;
                border-radius: 0.2em;
            }

            .w__charts__bar__bar {
                stroke: color-mix(in srgb, var(--color), #fff 25%);
                stroke-width: 1px;
            }

            /* Animations */

            .w__charts--animate-fade {
                animation: w__charts--fade 0.4s ease-out forwards;
            }

            @keyframes w__charts--fade {
                from {
                    opacity: 0;
                }
                to {
                    opacity: 1;
                }
            }

            .w__charts--animate-h-clip {
                animation: w__charts--h-clip 0.4s ease-out forwards;
            }

            @keyframes w__charts--h-clip {
                from {
                    clip-path: rect(0 0 0 0);
                }
                to {
                    clip-path: rect(0 100% 100% 0);
                }
            }

            .w__charts--animate-scale {
                transform: scale(0);
                animation: w__charts--scale 0.2s ease-out forwards;
            }

            @keyframes w__charts--scale {
                from {
                    transform: scale(0);
                }
                to {
                    transform: scale(1);
                }
            }

            .w__charts--animate-scale-z {
                transform: scale(1,0);
                animation: w__charts--scale-z 0.2s ease-out forwards;
            }

            @keyframes w__charts--scale-z {
                from {
                    transform: scale(1,0);
                }
                to {
                    transform: scale(1,1);
                }
            }

            /*

            .w__charts--y-axis path.domain,
            .w__charts--y-axis .tick line {
                display: none;
            }

            .w__charts--hover-line,
            .w__charts--hover-circle,
            .w__charts--tooltip {
                pointer-events: none;
            }

            .w__charts--hover-circle {
                filter: drop-shadow(0px 0px 8px currentColor);
            }

            .w__charts--hover-rect,
            .w__charts--hover-line,
            .w__charts--hover-circle,
            .w__charts--tooltip {
                opacity: 0;
            }

            .w__charts--hover-rect {
                fill: """ ++ W.Theme.Color.baseTintSubtle ++ """;
            }
            .w__charts--hover-rect.m--use-bars:hover,
            .w__charts--hover-rect:hover + g .w__charts--hover-line,
            .w__charts--hover-rect:hover + g .w__charts--hover-circle,
            .w__charts--hover-rect:hover + g + .w__charts--tooltip,
            .w__charts--tooltip-trigger:hover + .w__charts--tooltip {
                opacity: 1;
            }
            */
            """)
        ]

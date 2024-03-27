module W.Chart exposing
    ( globalStyles, fromX, fromXY, fromXYZ, ConfigX, ConfigXY, ConfigXYZ, Config, ChartAttribute
    , xAxis, axis, axisList
    , axisLabel, defaultValue, format, noAxisLine, noGridLines, safety, stacked, distribution, ticks, AxisAttribute
    , width, ratio, background
    , padding, paddingX, paddingY, paddingCustom
    , fontSize
    , view, WidgetX, WidgetXY, WidgetXYZ, Widget
    , withActive, withHover, groupByXY, onClick, onMouseEnter, onMouseLeave, PointX, PointXY, PointXYZ
    , Context, Coordinates, Point, RenderDatum, AxisDataPoints
    , debug
    )

{-|


# Setup

@docs globalStyles, fromX, fromXY, fromXYZ, ConfigX, ConfigXY, ConfigXYZ, Config, ChartAttribute


# Axis

@docs xAxis, axis, axisList


# Axis Attributes

@docs axisLabel, defaultValue, format, noAxisLine, noGridLines, safety, stacked, distribution, ticks, AxisAttribute


# Styles


## Size

@docs width, ratio, background


## Padding

@docs padding, paddingX, paddingY, paddingCustom


## Colors


## Font Sizes

@docs fontSize


# Widgets

@docs view, WidgetX, WidgetXY, WidgetXYZ, Widget


# Interaction

@docs withActive, withHover, groupByXY, onClick, onMouseEnter, onMouseLeave, PointX, PointXY, PointXYZ


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
import Scale
import Svg
import Svg.Attributes
import Svg.Events
import Theme
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC
import TypedSvg.Types as ST
import W.Chart.Internal
import W.Chart.Internal.Voronoi
import W.Svg.Attributes


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
type alias AxisConfigX x =
    AxisConfig x x { xAxis : Bool }


{-| -}
type alias AxisConfigYZ x y =
    AxisConfig x y {}


{-| -}
type AxisConfig x a constraint
    = AxisConfig
        W.Chart.Internal.AxisAttributes
        { data : List a
        , toLabel : a -> String
        , toColor : a -> String
        , toValue : a -> x -> Maybe Float
        }


{-| -}
type alias AxisAttribute =
    Attr.Attr W.Chart.Internal.AxisAttributes


{-| -}
type alias AxisDataPoints x a =
    W.Chart.Internal.AxisDataPoints x a


{-| -}
type alias HoverAttribute msg g =
    Attr.Attr (W.Chart.Internal.HoverAttrs msg g)


{-| -}
type alias Context x y z =
    W.Chart.Internal.Context x y z


{-| -}
type alias RenderDatum =
    W.Chart.Internal.RenderDatum



-- Config


{-| -}
fromX :
    List (ChartAttribute msg)
    -> { x : AxisConfigX x }
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
        { x : AxisConfigX x
        , y : AxisConfigYZ x y
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
        { x : AxisConfigX x
        , y : AxisConfigYZ x y
        , z : AxisConfigYZ x z
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
    Attr.attr
        (\a ->
            { a
                | padding =
                    { top = toFloat v
                    , left = toFloat v
                    , right = toFloat v
                    , bottom = toFloat v
                    }
            }
        )


{-| -}
paddingX : Int -> ChartAttribute msg
paddingX v =
    Attr.attr
        (\a ->
            let
                p : W.Chart.Internal.Padding
                p =
                    a.padding
            in
            { a
                | padding =
                    { p
                        | left = toFloat v
                        , right = toFloat v
                    }
            }
        )


{-| -}
paddingY : Int -> ChartAttribute msg
paddingY v =
    Attr.attr
        (\a ->
            let
                p : W.Chart.Internal.Padding
                p =
                    a.padding
            in
            { a
                | padding =
                    { p
                        | top = toFloat v
                        , bottom = toFloat v
                    }
            }
        )


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



-- Tooltips


{-| -}
withActive : Maybe Coordinates -> Config msg x y z point -> Config msg x y z point
withActive v (W.Chart.Internal.Config cfg) =
    W.Chart.Internal.Config { cfg | activePoint = v }


{-| -}
withHover : List (HoverAttribute msg point) -> Config msg x y z point -> Config msg x y z point
withHover =
    Attr.withAttrs
        { nearest = False
        , onClick = Nothing
        , onMouseEnter = Nothing
        , onMouseLeave = Nothing
        , custom = []
        }
        (\tooltipAttrs (W.Chart.Internal.Config cfg) ->
            W.Chart.Internal.Config { cfg | hover = Just tooltipAttrs }
        )


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


{-| -}
groupByXY : HoverAttribute msg point
groupByXY =
    Attr.attr (\a -> { a | nearest = True })



-- Axis


{-| -}
xAxis :
    List AxisAttribute
    ->
        { data : List x
        , toLabel : x -> String
        }
    -> AxisConfigX x
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
    List AxisAttribute
    ->
        { label : String
        , color : String
        , toValue : x -> Maybe Float
        }
    -> AxisConfigYZ x String
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
    List AxisAttribute
    ->
        { data : List a
        , toLabel : a -> String
        , toColor : a -> String
        , toValue : a -> x -> Maybe Float
        }
    -> AxisConfigYZ x a
axisList =
    Attr.withAttrs W.Chart.Internal.defaultAxisAttributes
        (\attrs props -> AxisConfig attrs props)



-- Axis Attributes


{-| -}
axisLabel : String -> AxisAttribute
axisLabel v =
    Attr.attr (\attrs -> { attrs | label = Just v })


{-| -}
defaultValue : Float -> AxisAttribute
defaultValue v =
    Attr.attr (\attrs -> { attrs | defaultValue = v })


{-| -}
format : (Float -> String) -> AxisAttribute
format v =
    Attr.attr (\attrs -> { attrs | format = v })


{-| -}
safety : Float -> AxisAttribute
safety v =
    Attr.attr (\attrs -> { attrs | safety = v })


{-| -}
ticks : Int -> AxisAttribute
ticks v =
    Attr.attr (\attrs -> { attrs | ticks = v })


{-| -}
stacked : AxisAttribute
stacked =
    Attr.attr (\attrs -> { attrs | stackType = W.Chart.Internal.Stacked })


{-| -}
distribution : AxisAttribute
distribution =
    Attr.attr (\attrs -> { attrs | stackType = W.Chart.Internal.Distribution })



-- TODO: Log scales doesn't work yet. Need to investigate it more.
-- logarithmic : Float -> AxisAttribute
-- logarithmic basis =
--     Attr.attr (\attrs -> { attrs | scale = W.Chart.Internal.Logarithmic basis })


{-| -}
noAxisLine : AxisAttribute
noAxisLine =
    Attr.attr (\attrs -> { attrs | showAxis = False })


{-| -}
noGridLines : AxisAttribute
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
                H.div
                    [ HA.class "ew-charts"
                    , HA.classList
                        [ ( "m--unfocus", True || cfg.hover /= Nothing )
                        , ( "m--debug", d.attrs.debug )
                        ]
                    , HA.attribute "style"
                        ("--ew-font-sm:"
                            ++ String.fromFloat d.ctx.fontSize.sm
                            ++ "px;--ew-font-md:"
                            ++ String.fromFloat d.ctx.fontSize.md
                            ++ "px;--ew-font-lg:"
                            ++ String.fromFloat d.ctx.fontSize.lg
                            ++ "px"
                        )
                    ]
                    [ Svg.svg
                        [ SA.viewBox 0 0 d.spacings.canvas.width d.spacings.canvas.height
                        , SA.class [ "ew-charts--svg" ]
                        ]
                        [ -- Grid
                          W.Chart.Internal.viewTranslateChart d.spacings
                            [ viewYGrid renderData
                            , viewXGrid d.ctx
                            ]

                        -- Labels
                        , viewLabels renderData

                        -- Axis
                        , viewYAxis renderData
                        , viewZAxis renderData

                        -- Elements & Hover
                        , W.Chart.Internal.viewTranslateChart d.spacings
                            [ viewXAxis d.ctx
                            , viewWidgets "bg" .background renderData widgets
                            , viewWidgets "main" .main renderData widgets
                            , viewWidgets "fg" .foreground renderData widgets
                            , viewActive cfg d.ctx widgets
                            , viewHover cfg d.ctx widgets
                            ]
                        ]
                    ]
            )
        |> Maybe.withDefault (H.text "")



-- Static Elements


viewWidgets :
    String
    -> (W.Chart.Internal.WidgetData msg x y z point -> Maybe (Context x y z -> Svg.Svg msg))
    -> W.Chart.Internal.RenderData msg x y z
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> SC.Svg msg
viewWidgets class getter (W.Chart.Internal.RenderData { ctx }) widgets =
    widgets
        |> List.filterMap (\(W.Chart.Internal.Widget w) -> Maybe.map (\el_ -> el_ ctx) (getter w))
        |> S.g [ SA.class [ "ew-charts--" ++ class ] ]



-- Hover Elements


viewActive :
    W.Chart.Internal.ConfigData msg x y z point
    -> W.Chart.Internal.Context x y z
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> SC.Svg msg
viewActive config ctx widgets =
    config.activePoint
        |> Maybe.andThen (\coords -> W.Chart.Internal.dataFromCoords coords ctx)
        |> Maybe.map
            (\point ->
                S.g [ Svg.Attributes.class "ew-charts--active" ]
                    (viewChartPoint ctx ( point, config.toPoint point ) widgets)
            )
        |> Maybe.withDefault (H.text "")


viewHover :
    W.Chart.Internal.ConfigData msg x y z point
    -> W.Chart.Internal.Context x y z
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> SC.Svg msg
viewHover config ctx widgets =
    config.hover
        |> Maybe.map
            (\hoverAttrs ->
                if hoverAttrs.nearest then
                    viewHoverNearest config hoverAttrs ctx widgets

                else
                    viewHoverX config hoverAttrs ctx widgets
            )
        |> Maybe.withDefault (H.text "")


viewHoverX :
    W.Chart.Internal.ConfigData msg x y z point
    -> W.Chart.Internal.HoverAttrs msg point
    -> W.Chart.Internal.Context x y z
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
    -> W.Chart.Internal.Context x y z
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
    W.Chart.Internal.Context x y z
    -> W.Chart.Internal.ChartPoint x y z
    -> point
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> SC.Svg msg
viewHoverContent ctx pointData point widgets =
    S.g
        [ SA.class [ "ew-charts--hover" ] ]
        (viewChartPointCoords ctx pointData :: viewChartPoint ctx ( pointData, point ) widgets)


viewChartPointCoords :
    W.Chart.Internal.Context x y z
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


viewChartPoint :
    W.Chart.Internal.Context x y z
    -> ( W.Chart.Internal.ChartPoint x y z, point )
    -> List (W.Chart.Internal.Widget msg x y z point)
    -> List (SC.Svg msg)
viewChartPoint ctx ( pointData, point ) widgets =
    [ viewHoverWidgets ctx pointData point widgets

    -- , if config.activePoint == Nothing then
    --     W.Chart.Internal.Tooltip.view ctx
    --         pointData
    --         tooltipMargin
    --         [ W.Chart.Internal.Tooltip.viewPoints ctx pointData ]
    --   else
    --     H.text ""
    ]


viewHoverAttrs : W.Chart.Internal.HoverAttrs msg point -> W.Chart.Internal.ChartPoint x y z -> point -> List (Svg.Attribute msg)
viewHoverAttrs hoverAttrs pointData point =
    [ SA.class [ "ew-charts--hover-target" ]
    , Svg.Attributes.fill "transparent"
    , W.Svg.Attributes.maybe hoverAttrs.onClick (\fn -> Svg.Events.onClick (fn pointData.pos point))
    , W.Svg.Attributes.maybe hoverAttrs.onMouseEnter (\fn -> Svg.Events.onMouseOver (fn pointData.pos point))
    , W.Svg.Attributes.maybe hoverAttrs.onMouseLeave (\fn -> Svg.Events.onMouseOut (fn pointData.pos point))
    ]


viewHoverWidgets :
    Context x y z
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



-- Labels


viewLabels : W.Chart.Internal.RenderData msg x y z -> SC.Svg msg
viewLabels (W.Chart.Internal.RenderData d) =
    S.g
        []
        [ d.attrs.yAxis.label
            |> W.Chart.Internal.maybeFilter (\_ -> W.Chart.Internal.isJust d.y)
            |> Maybe.map
                (\label ->
                    W.Chart.Internal.viewTranslate
                        { x = d.ctx.fontSize.lg * 1.75
                        , y = d.spacings.padding.top + d.spacings.chart.height * 0.5
                        }
                        [ S.text_
                            [ SA.transform [ ST.Rotate 270 0 0 ]
                            , SA.textAnchor ST.AnchorMiddle
                            , SAP.fontSize d.ctx.fontSize.lg
                            , SAP.x 0
                            , SAP.y 0
                            , Svg.Attributes.fill (Theme.baseForegroundWithAlpha 0.8)
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
                        { x = d.spacings.canvas.width - d.ctx.fontSize.lg * 1.75
                        , y = d.spacings.padding.top + d.spacings.chart.height * 0.5
                        }
                        [ S.text_
                            [ SA.transform [ ST.Rotate 90 0 0 ]
                            , SA.textAnchor ST.AnchorMiddle
                            , SAP.fontSize d.ctx.fontSize.lg
                            , SAP.x 0
                            , SAP.y 0
                            , Svg.Attributes.fill (Theme.baseForegroundWithAlpha 0.8)
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
                        , SAP.x (d.spacings.padding.left + d.spacings.chart.width * 0.5)
                        , SAP.y (d.spacings.canvas.height - d.ctx.fontSize.lg * 0.75)
                        , Svg.Attributes.fill (Theme.baseForegroundWithAlpha 0.8)
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
                            , Svg.Attributes.stroke (Theme.baseAuxWithAlpha 0.1)
                            ]
                            []
                    )
                |> S.g []

        _ ->
            H.text ""


viewXGrid : Context x y z -> SC.Svg msg
viewXGrid ctx =
    if ctx.x.showGrid then
        ctx.points.byX
            |> Dict.values
            |> toSteps ctx.x.ticks
            |> List.map
                (\{ x } ->
                    S.line
                        [ SAP.x1 x.render.valueScaled
                        , SAP.x2 x.render.valueScaled
                        , SAP.y1 0
                        , SAP.y2 ctx.height
                        , SA.strokeWidth (ST.px 1.0)
                        , Svg.Attributes.stroke (Theme.baseAuxWithAlpha 0.1)
                        ]
                        []
                )
            |> S.g []

    else
        H.text ""


toSteps : Int -> List a -> List a
toSteps ticks_ xs =
    let
        length : Int
        length =
            List.length xs

        mod : Int
        mod =
            ticks_
                |> min length
                |> max 1
                |> (\t -> length // t)
    in
    xs
        |> List.foldl
            (\x ( index, acc ) ->
                ( index + 1
                , if modBy mod index == 0 then
                    x :: acc

                  else
                    acc
                )
            )
            ( 0, [] )
        |> Tuple.second
        |> List.reverse


viewXAxis : Context x y z -> SC.Svg msg
viewXAxis ctx =
    if ctx.x.showAxis then
        S.g []
            [ S.line
                [ SAP.x1 0
                , SAP.x2 ctx.width
                , SAP.y1 0
                , SAP.y2 0
                , SAP.strokeWidth 1
                , Svg.Attributes.stroke (Theme.baseAuxWithAlpha 0.1)
                ]
                []
            , W.Chart.Internal.viewTranslate
                { x = 0
                , y = ctx.height
                }
                [ S.line
                    [ SAP.x1 0
                    , SAP.x2 ctx.width
                    , SAP.y1 0
                    , SAP.y2 0
                    , SAP.strokeWidth 1
                    , Svg.Attributes.stroke (Theme.baseAuxWithAlpha 0.2)
                    ]
                    []
                , ctx.points.byX
                    |> Dict.values
                    |> toSteps ctx.x.ticks
                    |> List.concatMap
                        (\xData ->
                            [ S.line
                                [ SAP.x1 xData.x.render.valueScaled
                                , SAP.x2 xData.x.render.valueScaled
                                , SAP.y1 0
                                , SAP.y2 6
                                , SAP.strokeWidth 1
                                , Svg.Attributes.stroke (Theme.baseAuxWithAlpha 0.2)
                                ]
                                []
                            , S.text_
                                [ SA.textAnchor ST.AnchorMiddle
                                , SAP.y (ctx.fontSize.sm * 2)
                                , SAP.x xData.x.render.valueScaled
                                , Svg.Attributes.fill (Theme.baseForegroundWithAlpha 0.6)
                                , SAP.fontSize ctx.fontSize.sm
                                ]
                                [ SC.text xData.x.render.label ]
                            ]
                        )
                    |> S.g []
                ]
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
            W.Chart.Internal.viewTranslate
                { x = d.spacings.padding.left
                , y = d.spacings.padding.top
                }
                [ S.g
                    [ SA.class [ "ew-charts--y-axis" ] ]
                    [ viewAxis
                        Axis.left
                        { ticks = d.ctx.y.ticks
                        , scale = d.ctx.y.scale
                        , format = axisFormat
                        }
                    ]
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
                { x = d.spacings.padding.left + d.spacings.chart.width
                , y = d.spacings.padding.top
                }
                [ S.g
                    [ SA.class [ "ew-charts--z-axis" ] ]
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

            .ew-charts {
                font-family: var(--theme-font-text), sans-serif;
            }

            /* Prevent Tooltip Clipping */

            .ew-charts--svg,
            .ew-charts--tooltip-wrapper {
                overflow: visible;
            }

            /* Unfocus */

            .ew-charts.m--unfocus:hover .ew-charts--main {
                filter: grayscale(0%);
            }

            /* Hover */

            .ew-charts--active,
            .ew-charts--hover {
                pointer-events: none;
            }
            .ew-charts--hover {
                display: none;
            }
            .ew-charts--hover-target:hover + .ew-charts--hover {
                display: block;
            }

            /* Debug */

            .ew-charts.m--debug .ew-charts--hover-target {
                fill: rgba(255, 0, 0, 0.05);
                stroke: rgba(255, 0, 0, 0.1);
            }
            .ew-charts.m--debug .ew-charts--hover-target:hover {
                fill: rgba(255, 0, 0, 0.05);
                stroke: rgba(255, 0, 0, 0.5);
            }

            /* Tooltip */

            .ew-charts--tooltip {
                display: flex;
                align-items: flex-end;
                justify-content: flex-start;
                box-sizing: border-box;
                font-family: inherit;
                font-size: var(--ew-font-md);
                line-height: 1;
                padding: 8px 0;
            }
            .ew-charts--tooltip.m--align-left {
                justify-content: flex-end;
            }
            .ew-charts--tooltip.m--align-top {
                align-items: flex-start;
            }
            .ew-charts--tooltip.m--align-center {
                align-items: center;
            }

            .ew-charts--tooltip-x,
            .ew-charts--tooltip-yz--label,
            .ew-charts--tooltip-yz--list,
            .ew-charts--tooltip-yz--item {
                margin: 0;
            }

            .ew-charts--tooltip-yz--label {
                display: flex;
                align-items: center;
                justify-content: space-between;
            }

            .ew-charts--tooltip-x {
                padding: 4px;
                font-weight: normal;
            }

            .ew-charts--tooltip-x-label {
                color: """ ++ Theme.baseAux ++ """;
            }

            .ew-charts--tooltip-x,
            .ew-charts--tooltip-yz--label {
                font-size: inherit;
            }

            .ew-charts--tooltip-x,
            .ew-charts--tooltip-yz--item {
                display: flex;
                align-items: center;
                justify-content: space-between;
                gap: 4px;
            }
            .ew-charts--tooltip-x--label,
            .ew-charts--tooltip-yz--item-label {
                flex-grow: 1;
            }

            .ew-charts--tooltip-x--value,
            .ew-charts--tooltip-yz--item-color,
            .ew-charts--tooltip-yz--item-value {
                flex-shrink: 0;
            }

            .ew-charts--tooltip-yz {
                border-top: 1px solid """ ++ Theme.baseAuxWithAlpha 0.1 ++ """;
                padding: 0 4px;
            }

            .ew-charts--tooltip-yz--label {
                padding: 4px 0 0;
            }

            .ew-charts--tooltip-yz--list {
                list-style-type: none;
                margin: 0;
                padding: 4px 0;
            }

            .ew-charts--tooltip-yz--item {
                padding: 2px 0;
                margin: 0;
            }
            .ew-charts--tooltip-yz--item-color {
                height: 8px;
                width: 8px;
                border-radius: 2px;
            }
            .ew-charts--tooltip-yz--item-label {
                padding: 0 8px 0 2px;
            }

            /* Axis & Labels */

            .ew-charts .tick text {
                fill: """ ++ Theme.baseAux ++ """;
                font-family: var(--theme-font-text), sans-serif;
                font-size: var(--ew-font-sm);
            }

            .ew-charts--x-axis path.domain,
            .ew-charts--y-axis path.domain,
            .ew-charts--z-axis path.domain {
                stroke: """ ++ Theme.baseAuxWithAlpha 0.1 ++ """;
                stroke-width: """ ++ String.fromFloat 1 ++ """px;
            }

            .ew-charts--x-axis .tick line,
            .ew-charts--y-axis .tick line,
            .ew-charts--z-axis .tick line {
                stroke: """ ++ Theme.baseAuxWithAlpha 0.2 ++ """;
            }

            /* Animations */

            .ew-charts--animate-fade {
                animation: ew-charts--fade 0.4s ease-out forwards;
            }

            @keyframes ew-charts--fade {
                from {
                    opacity: 0;
                }
                to {
                    opacity: 1;
                }
            }

            .ew-charts--animate-h-clip {
                animation: ew-charts--h-clip 0.4s ease-out forwards;
            }

            @keyframes ew-charts--h-clip {
                from {
                    clip-path: rect(0 0 0 0);
                }
                to {
                    clip-path: rect(0 100% 100% 0);
                }
            }

            .ew-charts--animate-scale {
                transform: scale(0);
                animation: ew-charts--scale 0.2s ease-out forwards;
            }

            @keyframes ew-charts--scale {
                from {
                    transform: scale(0);
                }
                to {
                    transform: scale(1);
                }
            }

            .ew-charts--animate-scale-z {
                transform: scale(1,0);
                animation: ew-charts--scale-z 0.2s ease-out forwards;
            }

            @keyframes ew-charts--scale-z {
                from {
                    transform: scale(1,0);
                }
                to {
                    transform: scale(1,1);
                }
            }

            /*
            .ew-charts--y-axis path.domain,
            .ew-charts--y-axis .tick line {
                display: none;
            }

            .ew-charts--hover-line,
            .ew-charts--hover-circle,
            .ew-charts--tooltip {
                pointer-events: none;
            }

            .ew-charts--hover-circle {
                filter: drop-shadow(0px 0px 8px currentColor);
            }

            .ew-charts--hover-rect,
            .ew-charts--hover-line,
            .ew-charts--hover-circle,
            .ew-charts--tooltip {
                opacity: 0;
            }

            .ew-charts--hover-rect {
                fill: """ ++ Theme.baseAuxWithAlpha 0.03 ++ """;
            }
            .ew-charts--hover-rect.m--use-bars:hover,
            .ew-charts--hover-rect:hover + g .ew-charts--hover-line,
            .ew-charts--hover-rect:hover + g .ew-charts--hover-circle,
            .ew-charts--hover-rect:hover + g + .ew-charts--tooltip,
            .ew-charts--tooltip-trigger:hover + .ew-charts--tooltip {
                opacity: 1;
            }
            */
            """)
        ]

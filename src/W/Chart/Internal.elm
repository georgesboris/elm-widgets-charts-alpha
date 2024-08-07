module W.Chart.Internal exposing
    ( Attribute(..)
    , Attributes
    , AxisAttributes
    , AxisConfig
    , AxisDataPoints
    , AxisDataPointsRender
    , AxisType(..)
    , ChartDatum
    , ChartPoint
    , ChartPointDict
    , Config(..)
    , ConfigData
    , Context
    , Coordinates
    , DataAttrs
    , DataPoint
    , HoverAttrs
    , HoverGrouping(..)
    , Padding
    , RenderAxisX
    , RenderAxisYZ
    , RenderData(..)
    , RenderDataFull
    , RenderDataX
    , RenderDataYZ
    , RenderDatum
    , ScaleType(..)
    , Spacings
    , StackType(..)
    , Widget(..)
    , WidgetData
    , applyAttrs
    , attrAnimationDelay
    , attrAnimationDelayX
    , attrTransformOrigin
    , bounds
    , boundsAt
    , dataFromCoords
    , defaultAttrs
    , defaultAxisAttributes
    , defaultHoverAttrs
    , formatFloat
    , formatPct
    , isJust
    , maybeAttr
    , maybeFilter
    , maybeIf
    , toAxis
    , toDataPointsRender
    , toRenderData
    , viewHtml
    , viewTranslate
    )

import Axis
import Dict exposing (values)
import Html as H
import Html.Attributes as HA
import Scale
import Set
import Shape
import Statistics
import Svg
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Core as SC
import TypedSvg.Types as ST
import W.Chart.Internal.Scale



-- DataPoint


type Widget msg x y z point
    = Widget (WidgetData msg x y z point)


type alias WidgetData msg x y z point =
    { main : Maybe (Context x y z -> Svg.Svg msg)
    , background : Maybe (Context x y z -> Svg.Svg msg)
    , foreground : Maybe (Context x y z -> Svg.Svg msg)
    , labels : Maybe (Context x y z -> Svg.Svg msg)
    , hover : Maybe (Context x y z -> Coordinates -> point -> Svg.Svg msg)
    }



-- Constants


dummyScale : Scale.ContinuousScale Float
dummyScale =
    Scale.linear ( 0, 0 ) ( 0, 0 )



-- Types


type Config msg x y z point
    = Config (ConfigData msg x y z point)


type alias ConfigData msg x y z point =
    { attrs : Attributes msg
    , activePoint : Maybe Coordinates
    , toPoint : ChartPoint x y z -> point
    , hover : Maybe (HoverAttrs msg point)
    , xData : Maybe (DataAttrs x x)
    , yData : Maybe (DataAttrs x y)
    , zData : Maybe (DataAttrs x z)
    }


type alias ChartDatum a =
    { datum : a
    , color : String
    , label : String
    }


type alias RenderAxisX x =
    { data : List x
    , scale : Axis.RenderableScale {} (List x) ( Float, Float ) x
    , binScale : Scale.BandScale x
    , ticks : Int
    , showAxis : Bool
    , showGrid : Bool
    , format : x -> String
    }


type alias RenderAxisYZ a =
    { data : List a
    , scale : Scale.ContinuousScale Float
    , zero : Float
    , label : Maybe String
    , isStacked : Bool
    , isDistribution : Bool
    , ticks : Int
    , format : Float -> String
    , formatStack : Maybe (List Float -> String)
    , showAxis : Bool
    , showGrid : Bool
    }


{-| -}
type alias Context x y z =
    { x : RenderAxisX x
    , y : RenderAxisYZ y
    , z : RenderAxisYZ z
    , points : ChartPointDict x y z
    , fontSize : { sm : Float, md : Float, lg : Float }
    , isDebugging : Bool
    , width : Float
    , height : Float
    }


{-| -}
type alias RenderDatum =
    { color : String
    , label : String
    , value : Float
    , valueString : String
    , valueScaled : Float
    , valueStart : Float
    , valueEnd : Float
    , isDefault : Bool
    }


{-| -}
type alias Coordinates =
    { x : Float, y : Maybe Float }


defaultHoverAttrs : HoverAttrs msg point
defaultHoverAttrs =
    { grouping = GroupByX
    , onClick = Nothing
    , onMouseEnter = Nothing
    , onMouseLeave = Nothing
    , custom = []
    }


type alias HoverAttrs msg point =
    { grouping : HoverGrouping
    , onClick : Maybe (Coordinates -> point -> msg)
    , onMouseEnter : Maybe (Coordinates -> point -> msg)
    , onMouseLeave : Maybe (Coordinates -> point -> msg)
    , custom : List (H.Html msg)
    }


type HoverGrouping
    = GroupByX
    | GroupByXY


type alias DataAttrs x a =
    { data : List a
    , toLabel : a -> String
    , toColor : a -> String
    , toValue : a -> x -> Maybe Float
    }


type RenderData msg x y z
    = RenderData (RenderDataFull msg x y z)


type alias RenderDataFull msg x y z =
    { attrs : Attributes msg
    , spacings : Spacings
    , x : RenderDataX x
    , y : Maybe (RenderDataYZ x y)
    , z : Maybe (RenderDataYZ x z)
    , ctx : Context x y z
    , points : ChartPointDict x y z
    }


dataFromCoords : Coordinates -> Context x y z -> Maybe (ChartPoint x y z)
dataFromCoords coords ctx =
    case coords.y of
        Just y ->
            Dict.get ( coords.x, y ) ctx.points.byXY

        Nothing ->
            Dict.get coords.x ctx.points.byX


type alias AxisRenderDatum =
    { color : String, label : String }


type alias AxisDataPoints x a =
    ( ChartDatum a, List ( DataPoint x, DataPoint a ) )


type alias AxisDataPointsRender =
    ( AxisRenderDatum, List ( RenderDatum, RenderDatum ) )


toDataPointsRender : AxisDataPoints x a -> AxisDataPointsRender
toDataPointsRender ( chartDatum, xs ) =
    ( { color = chartDatum.color, label = chartDatum.label }, List.map (Tuple.mapBoth .render .render) xs )


type alias ChartPointDict x y z =
    { y : List (AxisDataPoints x y)
    , z : List (AxisDataPoints x z)
    , byX : Dict.Dict Float (ChartPoint x y z)
    , byXY : Dict.Dict ( Float, Float ) (ChartPoint x y z)
    }


type alias ChartPoint x y z =
    { pos : { x : Float, y : Maybe Float }
    , x : DataPoint x
    , y : List (DataPoint y)
    , z : List (DataPoint z)
    , xRender : RenderDatum
    , yRender : List RenderDatum
    , zRender : List RenderDatum
    }


type alias DataPoint a =
    { datum : ChartDatum a
    , render : RenderDatum
    }


type alias RenderDataX x =
    { data : List (ChartDatum x)
    , toLabel : x -> String
    , toColor : x -> String
    , toValue : x -> x -> Maybe Float
    , scale : Axis.RenderableScale {} (List x) ( Float, Float ) x
    , bandScale : Scale.BandScale x
    }


type alias RenderDataYZ x a =
    { data : List a
    , toLabel : a -> String
    , toColor : a -> String
    , toValue : a -> x -> Maybe Float
    , scale : Scale.ContinuousScale Float
    , stack : Shape.StackResult (ChartDatum a)
    , isStacked : Bool
    , bandData : List ( ChartDatum a, List ( Float, Float ) )
    , values :
        List
            { datum : ChartDatum a
            , values : List (DataPoint a)
            , stackedValues : List ( Float, Float )
            }
    }


toRenderData : ConfigData msg x y z g -> DataAttrs x x -> RenderData msg x y z
toRenderData cfg xData =
    let
        spacings : Spacings
        spacings =
            toSpacings cfg.attrs

        bandScale : Scale.BandScale x
        bandScale =
            Scale.band
                { paddingOuter = 0.0
                , paddingInner = 0.0
                , align = 0.5
                }
                ( 0, spacings.chart.width )
                xData.data

        x : RenderDataX x
        x =
            { data = List.map (toChartDatum xData) xData.data
            , toLabel = xData.toLabel
            , toColor = xData.toColor
            , toValue = xData.toValue
            , bandScale = bandScale
            , scale = Scale.toRenderable xData.toLabel bandScale
            }

        yBeforeNormalization : Maybe (RenderDataYZ x y)
        yBeforeNormalization =
            cfg.yData
                |> Maybe.map
                    (\yData_ ->
                        toStackedData
                            { spacings = spacings
                            , xData = xData
                            , axisData = yData_
                            , axisConfig = cfg.attrs.yAxis
                            }
                    )

        zBeforeNormalization : Maybe (RenderDataYZ x z)
        zBeforeNormalization =
            cfg.zData
                |> Maybe.map
                    (\zData_ ->
                        toStackedData
                            { spacings = spacings
                            , xData = xData
                            , axisData = zData_
                            , axisConfig = cfg.attrs.zAxis
                            }
                    )

        -- If both X and Z are defined
        -- we normalize them so their 0.0 match.
        ( yScale, zScale ) =
            case ( ( yBeforeNormalization, cfg.attrs.yAxis.scale ), ( zBeforeNormalization, cfg.attrs.zAxis.scale ) ) of
                ( ( Just y, Linear ), ( Just z, Linear ) ) ->
                    W.Chart.Internal.Scale.normalizeDomains
                        (Scale.domain y.scale)
                        (Scale.domain z.scale)
                        |> Tuple.mapBoth
                            (toScale spacings cfg.attrs.yAxis)
                            (toScale spacings cfg.attrs.zAxis)

                _ ->
                    ( yBeforeNormalization
                        |> Maybe.map (\y -> toScale spacings cfg.attrs.yAxis (Scale.domain y.scale))
                        |> Maybe.withDefault dummyScale
                    , zBeforeNormalization
                        |> Maybe.map (\z -> toScale spacings cfg.attrs.zAxis (Scale.domain z.scale))
                        |> Maybe.withDefault dummyScale
                    )

        yData : Maybe (RenderDataYZ x y)
        yData =
            Maybe.map (scaleStackedData yScale) yBeforeNormalization

        zData : Maybe (RenderDataYZ x z)
        zData =
            Maybe.map (scaleStackedData zScale) zBeforeNormalization

        yDataList : List y
        yDataList =
            yData
                |> Maybe.map .data
                |> Maybe.withDefault []

        zDataList : List z
        zDataList =
            zData
                |> Maybe.map .data
                |> Maybe.withDefault []

        points : ChartPointDict x y z
        points =
            toChartPointDict cfg.attrs x yData zData
    in
    RenderData
        { attrs = cfg.attrs
        , spacings = spacings
        , x = x
        , y = yData
        , z = zData
        , points = points
        , ctx =
            { isDebugging = cfg.attrs.debug
            , width = spacings.chart.width
            , height = spacings.chart.height
            , points = points
            , fontSize =
                { sm = cfg.attrs.fontSize.small
                , md = cfg.attrs.fontSize.medium
                , lg = cfg.attrs.fontSize.large
                }
            , x =
                { data = xData.data
                , scale = x.scale
                , binScale = x.bandScale
                , ticks = cfg.attrs.xAxis.ticks
                , showAxis = cfg.attrs.xAxis.showAxis
                , showGrid = cfg.attrs.xAxis.showGrid
                , format = xData.toLabel
                }
            , y =
                { data = yDataList
                , scale = yScale
                , zero = Scale.convert yScale 0
                , isStacked = cfg.attrs.yAxis.stackType /= NotStacked
                , isDistribution = cfg.attrs.yAxis.stackType == Distribution
                , ticks = cfg.attrs.yAxis.ticks
                , label = cfg.attrs.yAxis.label
                , format = cfg.attrs.yAxis.format
                , formatStack = cfg.attrs.yAxis.formatStack
                , showAxis = cfg.attrs.yAxis.showAxis
                , showGrid = cfg.attrs.yAxis.showGrid
                }
            , z =
                { data = zDataList
                , scale = zScale
                , zero = Scale.convert zScale 0
                , isStacked = cfg.attrs.zAxis.stackType /= NotStacked
                , isDistribution = cfg.attrs.zAxis.stackType == Distribution
                , ticks = cfg.attrs.yAxis.ticks
                , label = cfg.attrs.zAxis.label
                , format = cfg.attrs.zAxis.format
                , formatStack = cfg.attrs.zAxis.formatStack
                , showAxis = cfg.attrs.zAxis.showAxis
                , showGrid = cfg.attrs.zAxis.showGrid
                }
            }
        }


toScale : Spacings -> AxisAttributes -> ( Float, Float ) -> Scale.ContinuousScale Float
toScale spacings axisAttributes domain =
    domain
        |> toDomainWithSafety axisAttributes
        |> toScaleFn spacings axisAttributes


toScaleFn : Spacings -> AxisAttributes -> ( Float, Float ) -> Scale.ContinuousScale Float
toScaleFn spacings axisAttributes domain =
    case axisAttributes.scale of
        Linear ->
            Scale.linear ( spacings.chart.height, 0 ) domain

        Logarithmic base ->
            Scale.log base ( spacings.chart.height, 0 ) domain


toDomainWithSafety : AxisAttributes -> ( Float, Float ) -> ( Float, Float )
toDomainWithSafety axisAttributes domain =
    safeBounds axisAttributes.safety domain


toValuesByX : List (DataPoint x) -> Maybe (RenderDataYZ x a) -> List (List (DataPoint a))
toValuesByX xPoints yzData =
    case
        yzData
            |> Maybe.map .values
            |> maybeFilter (not << List.isEmpty)
            |> Maybe.map (List.map .values >> transpose)
    of
        Just valuesByX ->
            valuesByX

        Nothing ->
            List.map (\_ -> []) xPoints


toAxisPoints : List (DataPoint x) -> RenderDataYZ x a -> List ( ChartDatum a, List ( DataPoint x, DataPoint a ) )
toAxisPoints xPoints axisData =
    axisData.values
        |> List.map
            (\yData ->
                ( yData.datum
                , List.map2
                    Tuple.pair
                    xPoints
                    yData.values
                )
            )


toChartPointDict :
    Attributes msg
    -> RenderDataX x
    -> Maybe (RenderDataYZ x y)
    -> Maybe (RenderDataYZ x z)
    -> ChartPointDict x y z
toChartPointDict attrs xData maybeYData maybeZData =
    let
        xPoints : List (DataPoint x)
        xPoints =
            xData.data
                |> List.map
                    (\x ->
                        let
                            xScaled : Float
                            xScaled =
                                Scale.convert xData.scale x.datum

                            xBin : Float
                            xBin =
                                Scale.convert xData.bandScale x.datum
                        in
                        { datum = x
                        , render =
                            { color = x.color
                            , label = x.label
                            , value = Scale.convert xData.scale x.datum
                            , valueString = attrs.xAxis.format xScaled
                            , valueScaled = xScaled
                            , valueStart = xBin
                            , valueEnd = xBin + Scale.bandwidth xData.bandScale
                            , isDefault = False
                            }
                        }
                    )

        pointsBase :
            List
                ( DataPoint x
                , List (DataPoint y)
                , List (DataPoint z)
                )
        pointsBase =
            List.map3 (\x yForX zForX -> ( x, yForX, zForX ))
                xPoints
                (toValuesByX xPoints maybeYData)
                (toValuesByX xPoints maybeZData)
    in
    pointsBase
        |> List.foldl
            (\( x, ys, zs ) ( byX, byXZ ) ->
                let
                    ysByY : Dict.Dict Float (List (DataPoint y))
                    ysByY =
                        toByY ys

                    zsByY : Dict.Dict Float (List (DataPoint z))
                    zsByY =
                        toByY zs

                    yValues : List Float
                    yValues =
                        Dict.keys ysByY
                            |> Set.fromList
                            |> Set.union (Set.fromList (Dict.keys zsByY))
                            |> Set.toList
                in
                ( ( x.render.valueScaled
                  , { pos = { x = x.render.valueScaled, y = Nothing }
                    , x = x
                    , y = ys
                    , z = zs
                    , xRender = x.render
                    , yRender = List.map .render ys
                    , zRender = List.map .render zs
                    }
                  )
                    :: byX
                , yValues
                    |> List.map
                        (\yValue ->
                            let
                                ys_ : List (DataPoint y)
                                ys_ =
                                    Dict.get yValue ysByY
                                        |> Maybe.withDefault []

                                zs_ : List (DataPoint z)
                                zs_ =
                                    Dict.get yValue zsByY
                                        |> Maybe.withDefault []
                            in
                            ( ( x.render.valueScaled, yValue )
                            , { pos = { x = x.render.valueScaled, y = Just yValue }
                              , x = x
                              , y = ys_
                              , z = zs_
                              , xRender = x.render
                              , yRender = List.map .render ys_
                              , zRender = List.map .render zs_
                              }
                            )
                        )
                    |> List.append byXZ
                )
            )
            ( [], [] )
        |> (\( byX, byXY ) ->
                { y =
                    maybeYData
                        |> Maybe.map (toAxisPoints xPoints)
                        |> Maybe.withDefault []
                , z =
                    maybeZData
                        |> Maybe.map (toAxisPoints xPoints)
                        |> Maybe.withDefault []
                , byX = Dict.fromList byX
                , byXY = Dict.fromList byXY
                }
           )


toChartDatum : DataAttrs x a -> a -> ChartDatum a
toChartDatum dataAttrs a =
    { datum = a
    , color = dataAttrs.toColor a
    , label = dataAttrs.toLabel a
    }



-- |> Dict.fromList


toByY : List (DataPoint a) -> Dict.Dict Float (List (DataPoint a))
toByY dataPoints =
    dataPoints
        |> List.foldl
            (\point acc ->
                addToList point.render.valueStart point acc
            )
            Dict.empty
        |> Dict.map (\_ -> List.reverse)



-- Spacings


type alias Spacings =
    { canvas :
        { width : Float
        , height : Float
        }
    , chart :
        { width : Float
        , height : Float
        }
    , padding :
        { top : Float
        , bottom : Float
        , left : Float
        , right : Float
        }
    }


type Attribute msg
    = Attribute (Attributes msg -> Attributes msg)


type alias Attributes msg =
    { debug : Bool
    , labels : Bool
    , labelsAutoHide : Bool
    , width : Float
    , ratio : Float
    , fontSize :
        { small : Float
        , medium : Float
        , large : Float
        }
    , xAxis : AxisAttributes
    , yAxis : AxisAttributes
    , zAxis : AxisAttributes
    , padding :
        { top : Float
        , bottom : Float
        , left : Float
        , right : Float
        }
    , background : String
    , id : Maybe String
    , onMouseEnter : Maybe msg
    , onMouseLeave : Maybe msg
    }


type ScaleType
    = Linear
    | Logarithmic Float


type StackType
    = NotStacked
    | Stacked
    | Distribution


type AxisType
    = AxisX
    | AxisY
    | AxisZ


type alias AxisAttributes =
    { label : Maybe String
    , defaultValue : Float
    , format : Float -> String
    , formatStack : Maybe (List Float -> String)
    , tooltipFormat : Maybe (Float -> String)
    , safety : Float
    , ticks : Int
    , scale : ScaleType
    , stackType : StackType
    , showAxis : Bool
    , showGrid : Bool
    }


type alias AxisConfig =
    { label : Maybe String
    , default : Float
    , format : Float -> String
    , safety : Float
    , scale : ScaleType
    , stackType : StackType
    , ticks : Int
    , showAxis : Bool
    , showGrid : Bool
    }


toAxis : AxisType -> Attributes msg -> AxisAttributes
toAxis axisType attrs =
    case axisType of
        AxisX ->
            attrs.xAxis

        AxisY ->
            attrs.yAxis

        AxisZ ->
            attrs.zAxis


defaultAxisAttributes : AxisAttributes
defaultAxisAttributes =
    { label = Nothing
    , defaultValue = 0.0
    , format = formatFloat
    , formatStack = Nothing
    , tooltipFormat = Nothing
    , safety = 0.1
    , ticks = 5
    , scale = Linear
    , stackType = NotStacked
    , showAxis = True
    , showGrid = True
    }


defaultAttrs : Attributes msg
defaultAttrs =
    { debug = False
    , labels = True
    , labelsAutoHide = True
    , width = 960
    , ratio = 0.5
    , fontSize =
        { small = 12
        , medium = 13
        , large = 14
        }
    , xAxis = defaultAxisAttributes
    , yAxis = defaultAxisAttributes
    , zAxis = defaultAxisAttributes
    , padding =
        { top = 28
        , left = 80
        , right = 80
        , bottom = 64
        }
    , background = "transparent"
    , id = Nothing
    , onMouseEnter = Nothing
    , onMouseLeave = Nothing
    }


type alias Padding =
    { top : Float
    , right : Float
    , left : Float
    , bottom : Float
    }


formatFloat : Float -> String
formatFloat value =
    let
        valueString : String
        valueString =
            String.fromFloat value
    in
    case String.split "." valueString of
        int :: dec :: [] ->
            int ++ "." ++ String.left 2 dec

        int :: [] ->
            int

        _ ->
            valueString


formatPct : Float -> String
formatPct value =
    value
        * 100
        |> formatFloat
        |> (\s -> s ++ "%")


applyAttrs : List (Attribute msg) -> Attributes msg
applyAttrs attrs =
    List.foldl (\(Attribute fn) a -> fn a) defaultAttrs attrs



-- Helpers : StackedData


scaleStackedData : Scale.ContinuousScale Float -> RenderDataYZ x a -> RenderDataYZ x a
scaleStackedData scale yz =
    { yz
        | scale = scale
        , values =
            yz.values
                |> List.map
                    (\axis ->
                        { axis
                            | values =
                                axis.values
                                    |> List.map
                                        (\point ->
                                            let
                                                render : RenderDatum
                                                render =
                                                    point.render

                                                startScaled : Float
                                                startScaled =
                                                    Scale.convert scale render.valueStart

                                                endScaled : Float
                                                endScaled =
                                                    Scale.convert scale render.valueEnd

                                                valueScaled : Float
                                                valueScaled =
                                                    if yz.isStacked then
                                                        if render.valueStart > 0.0 then
                                                            startScaled

                                                        else
                                                            endScaled

                                                    else
                                                        Scale.convert scale render.valueScaled
                                            in
                                            { point
                                                | render =
                                                    { render
                                                        | valueScaled = valueScaled
                                                        , valueStart = startScaled
                                                        , valueEnd = endScaled
                                                    }
                                            }
                                        )
                        }
                    )
    }


toStackedData :
    { spacings : Spacings
    , xData : DataAttrs x x
    , axisData : DataAttrs x a
    , axisConfig : AxisAttributes
    }
    -> RenderDataYZ x a
toStackedData props =
    let
        dataWithValues : List ( ChartDatum a, List Float )
        dataWithValues =
            props.axisData.data
                |> List.map
                    (\a ->
                        let
                            values : List Float
                            values =
                                props.xData.data
                                    |> List.map
                                        (\bin ->
                                            props.axisData.toValue a bin
                                                |> Maybe.withDefault props.axisConfig.defaultValue
                                        )
                        in
                        ( { datum = a
                          , color = props.axisData.toColor a
                          , label = props.axisData.toLabel a
                          }
                        , values
                        )
                    )

        stack : Shape.StackResult (ChartDatum a)
        stack =
            let
                stackOffset : List (List ( Float, Float )) -> List (List ( Float, Float ))
                stackOffset =
                    case props.axisConfig.stackType of
                        NotStacked ->
                            identity

                        Stacked ->
                            Shape.stackOffsetDiverging

                        Distribution ->
                            stackOffsetDistribution
                                >> Shape.stackOffsetDiverging
            in
            Shape.stack
                { offset = stackOffset
                , order = identity
                , data = dataWithValues
                }

        scale : Scale.ContinuousScale Float
        scale =
            toScaleFn props.spacings props.axisConfig stack.extent
    in
    { data = props.axisData.data
    , bandData = List.map2 Tuple.pair stack.labels stack.values
    , toLabel = props.axisData.toLabel
    , toColor = props.axisData.toColor
    , toValue = props.axisData.toValue
    , scale = scale
    , isStacked = props.axisConfig.stackType /= NotStacked
    , stack = stack
    , values =
        List.map2
            (\( a, values ) stackedValues ->
                { datum =
                    { datum = a.datum
                    , color = a.color
                    , label = a.label
                    }
                , values =
                    List.map3
                        (\x v ( end, start ) ->
                            { datum =
                                { datum = a.datum
                                , color = a.color
                                , label = a.label
                                }
                            , render =
                                { color = a.color
                                , label = a.label
                                , value = v
                                , valueString = props.axisConfig.format v
                                , valueScaled = v
                                , valueStart = start
                                , valueEnd = end
                                , isDefault = props.axisData.toValue a.datum x == Nothing
                                }
                            }
                        )
                        props.xData.data
                        values
                        stackedValues
                , stackedValues = stackedValues
                }
            )
            dataWithValues
            stack.values
    }


stackOffsetDistribution : List (List ( Float, Float )) -> List (List ( Float, Float ))
stackOffsetDistribution series =
    series
        |> transpose
        |> List.map normalizeColumn
        |> transpose


normalizeColumn : List ( Float, Float ) -> List ( Float, Float )
normalizeColumn column =
    let
        values : List Float
        values =
            List.map (\( a, b ) -> b - a) column

        total : Float
        total =
            List.foldl (\x acc -> acc + abs x) 0 values
    in
    if total == 0 then
        List.map (\_ -> ( 0, 0 )) values

    else
        List.map (\value -> ( 0, value / total )) values


transpose : List (List a) -> List (List a)
transpose listOfLists =
    List.foldr (List.map2 (::)) (List.repeat (rowsLength listOfLists) []) listOfLists


rowsLength : List (List a) -> Int
rowsLength listOfLists =
    case listOfLists of
        [] ->
            0

        x :: _ ->
            List.length x



-- Helpers : Spacings


toSpacings : Attributes msg -> Spacings
toSpacings attrs =
    let
        canvas : { width : Float, height : Float }
        canvas =
            { width = attrs.width
            , height = attrs.width * attrs.ratio
            }

        chart : { width : Float, height : Float }
        chart =
            { height = canvas.height - attrs.padding.top - attrs.padding.bottom
            , width = canvas.width - attrs.padding.left - attrs.padding.right
            }
    in
    { padding = attrs.padding
    , canvas =
        { width = canvas.width
        , height = canvas.height
        }
    , chart =
        { width = chart.width
        , height = chart.height
        }
    }



-- Helpers : Bounds


bounds : List Float -> ( Float, Float )
bounds data =
    Statistics.extent data
        |> Maybe.withDefault ( 0, 0 )


{-| -}
safeBounds : Float -> ( Float, Float ) -> ( Float, Float )
safeBounds safety ( min, max ) =
    let
        -- If we have the same min and max for a given axis,
        -- that problably means we only have one data point.
        -- If that happens we will use the `min` value as the delta and expand the canvas
        -- so we prevent cases where the grid is confusing since the whole axis is made of a single value.
        baseValue : Float
        baseValue =
            if max == min then
                min

            else if max > 0 && min < 0 then
                max - min

            else if max > 0 then
                max

            else
                abs min

        minSafety : Float
        minSafety =
            min - (baseValue * safety)

        maxSafety : Float
        maxSafety =
            max + (baseValue * safety)

        -- If the `minSafety` would cause the axis to show negative values,
        -- even though there are no negative data points, then we clamp it to zero.
        minClampedSafety : Float
        minClampedSafety =
            if minSafety < 0.0 && min >= 0.0 then
                0.0

            else
                minSafety

        -- Following the same logic, if the `maxSafety` would cause the axis to show positive values,
        -- even though there are no positive data points, then we clamp it to zero.
        maxClampedSafety : Float
        maxClampedSafety =
            if maxSafety > 0.0 && max <= 0.0 then
                0.0

            else
                maxSafety
    in
    ( minClampedSafety
    , maxClampedSafety
    )


boundsAt : (a -> number) -> List a -> Maybe { max : number, min : number }
boundsAt toA xs =
    case xs of
        x :: xs_ ->
            List.foldl
                (\v ( max, min ) ->
                    ( Basics.max max (toA v)
                    , Basics.min min (toA v)
                    )
                )
                ( toA x, toA x )
                xs_
                |> (\( max, min ) ->
                        { max = max
                        , min = min
                        }
                   )
                |> Just

        [] ->
            Nothing



-- Helpers : Views


viewTranslate : { x : Float, y : Float } -> List (SC.Svg msg) -> SC.Svg msg
viewTranslate props children =
    S.g [ SA.transform [ ST.Translate props.x props.y ] ]
        children


viewHtml : List (Svg.Attribute msg) -> List (Svg.Svg msg) -> SC.Svg msg
viewHtml attrs children =
    Svg.foreignObject attrs
        [ H.div [ HA.attribute "xlmns" "http://www.w3.org/1999/xhtml" ] children
        ]



---


attrAnimationDelayX : Context x y z -> Float -> Svg.Attribute msg
attrAnimationDelayX ctx xScaled =
    let
        -- This percentage based on both X and Y creates an offset
        -- that makes points on the lower left appear sooner
        -- than points on the upper right
        pct : Float
        pct =
            xScaled / ctx.width

        -- Controls the max offset
        -- The faster points will have 0.0 offset and
        -- the lowest points will be offset by this amount
        maxDelay : Float
        maxDelay =
            0.3

        delay : Float
        delay =
            maxDelay * pct
    in
    SA.style ("animation-delay:" ++ String.fromFloat delay ++ "s")


attrAnimationDelay : Context x y z -> Float -> Float -> Svg.Attribute msg
attrAnimationDelay ctx xScaled yScaled =
    let
        -- This percentage based on both X and Y creates an offset
        -- that makes points on the lower left appear sooner
        -- than points on the upper right
        pct : Float
        pct =
            0.5 * ((xScaled / ctx.width) + (yScaled / ctx.height))

        -- Controls the max offset
        -- The faster points will have 0.0 offset and
        -- the lowest points will be offset by this amount
        maxDelay : Float
        maxDelay =
            0.3

        delay : Float
        delay =
            maxDelay * pct
    in
    SA.style ("animation-delay:" ++ String.fromFloat delay ++ "s")


attrTransformOrigin : Float -> Float -> Svg.Attribute msg
attrTransformOrigin cx cy =
    HA.attribute
        "transform-origin"
        (String.fromFloat cx ++ " " ++ String.fromFloat cy)



---


maybeAttr : Maybe a -> (a -> H.Attribute msg) -> H.Attribute msg
maybeAttr m fn =
    case m of
        Just a ->
            fn a

        Nothing ->
            HA.class ""


isJust : Maybe a -> Bool
isJust m =
    m /= Nothing


maybeIf : (a -> Bool) -> a -> Maybe a
maybeIf predicate m =
    if predicate m then
        Just m

    else
        Nothing


maybeFilter : (a -> Bool) -> Maybe a -> Maybe a
maybeFilter predicate =
    Maybe.andThen
        (\a ->
            if predicate a then
                Just a

            else
                Nothing
        )


addToList : comparable -> v -> Dict.Dict comparable (List v) -> Dict.Dict comparable (List v)
addToList k v =
    Dict.update
        k
        (\maybeList ->
            maybeList
                |> Maybe.map (\vs -> v :: vs)
                |> Maybe.withDefault [ v ]
                |> Just
        )

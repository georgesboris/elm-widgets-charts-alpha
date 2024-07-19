module W.Chart.Bar exposing
    ( fromY, fromZ, fromYZ
    , margins
    , labelFormat, labelFormatWithList, labelsAsPercentages, labelsOutside
    , onClick, onMouseEnter, onMouseLeave, EventTarget(..)
    , Attribute
    )

{-|

@docs fromY, fromZ, fromYZ

@docs margins
@docs labelFormat, labelFormatWithList, labelsAsPercentages, labelsOutside
@docs onClick, onMouseEnter, onMouseLeave, EventTarget
@docs Attribute

-}

import Attr
import Dict
import Scale
import Svg
import Svg.Attributes
import Svg.Events as SE
import TypedSvg as S
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC
import W.Chart
import W.Chart.Internal
import W.Chart.Widget
import W.Chart.Widget.Label



-- Attributes


{-| -}
type EventTarget y z
    = YDatum y
    | ZDatum z


{-| -}
type alias Attribute a msg =
    Attr.Attr (Attributes a msg)


type alias Attributes a msg =
    { outerMargin : Float
    , innerMargin : Float
    , labelPosition : W.Chart.Widget.Label.Attribute
    , labelFormat : W.Chart.Widget.Label.Attribute
    , onClick : Maybe (a -> msg)
    , onMouseEnter : Maybe (a -> msg)
    , onMouseLeave : Maybe (a -> msg)
    }


defaultAttrs : Attributes a msg
defaultAttrs =
    { innerMargin = 0.2
    , outerMargin = 0.5
    , labelPosition = W.Chart.Widget.Label.inside
    , labelFormat = Attr.none
    , onClick = Nothing
    , onMouseEnter = Nothing
    , onMouseLeave = Nothing
    }


{-| -}
margins : Float -> Float -> Attribute a msg
margins innerMargin outerMargin =
    Attr.attr (\attr -> { attr | innerMargin = innerMargin, outerMargin = outerMargin })


{-| -}
labelsOutside : Attribute a msg
labelsOutside =
    Attr.attr (\attr -> { attr | labelPosition = Attr.none })


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



-- Views


{-| -}
fromY : List (Attribute y msg) -> W.Chart.WidgetXY msg x y z a
fromY =
    Attr.withAttrs defaultAttrs
        (\attrs ->
            W.Chart.Widget.fromY
                (\ctx ->
                    viewBars
                        ctx
                        ctx.y
                        attrs
                        (toBinScale attrs ctx (binCount ctx.y))
                        (toIndexed ctx.y 0 ctx.points.y)
                        identity
                )
                |> W.Chart.Widget.withLabels
                    (\ctx ->
                        let
                            yCount : Int
                            yCount =
                                binCount ctx.y

                            binScale : Scale.BandScale Int
                            binScale =
                                toBinScale attrs ctx yCount
                        in
                        W.Chart.Widget.Label.viewBinsList
                            [ attrs.labelPosition
                            , attrs.labelFormat
                            ]
                            { binScale = binScale
                            , ctx = ctx
                            , points =
                                ctx.points.byX
                                    |> Dict.values
                                    |> List.map
                                        (\data ->
                                            { x = data.x
                                            , y = data.y
                                            , z = []
                                            }
                                        )
                            }
                    )
                |> W.Chart.Widget.withHover
                    (\ctx ->
                        \_ point ->
                            viewHover
                                ctx.y
                                (toBinScale attrs ctx (binCount ctx.y))
                                point.x.render
                                (toIndexedMap .render ctx.y 0 point.y)
                    )
        )


{-| -}
fromZ : List (Attribute z msg) -> W.Chart.WidgetXYZ msg x y z a
fromZ =
    Attr.withAttrs defaultAttrs
        (\attrs ->
            W.Chart.Widget.fromZ
                (\ctx ->
                    viewBars
                        ctx
                        ctx.z
                        attrs
                        (toBinScale attrs ctx (binCount ctx.z))
                        (toIndexed ctx.z 0 ctx.points.z)
                        identity
                )
                |> W.Chart.Widget.withLabels
                    (\ctx ->
                        let
                            zCount : Int
                            zCount =
                                binCount ctx.z

                            binScale : Scale.BandScale Int
                            binScale =
                                toBinScale attrs ctx zCount
                        in
                        W.Chart.Widget.Label.viewBinsList
                            [ attrs.labelPosition
                            , attrs.labelFormat
                            ]
                            { binScale = binScale
                            , ctx = ctx
                            , points =
                                ctx.points.byX
                                    |> Dict.values
                                    |> List.map
                                        (\data ->
                                            { x = data.x
                                            , z = data.z
                                            , y = []
                                            }
                                        )
                            }
                    )
                |> W.Chart.Widget.withHover
                    (\ctx ->
                        \_ point ->
                            viewHover ctx.z
                                (toBinScale attrs ctx (binCount ctx.z))
                                point.x.render
                                (toIndexedMap .render ctx.z 0 point.z)
                    )
        )


{-| -}
fromYZ : List (Attribute (EventTarget y z) msg) -> W.Chart.WidgetXYZ msg x y z a
fromYZ =
    Attr.withAttrs defaultAttrs
        (\attrs ->
            W.Chart.Widget.fromYZ
                (\ctx ->
                    let
                        yCount : Int
                        yCount =
                            binCount ctx.y

                        zCount : Int
                        zCount =
                            binCount ctx.z

                        binScale : Scale.BandScale Int
                        binScale =
                            toBinScale attrs ctx (yCount + zCount)
                    in
                    S.g []
                        [ viewBars ctx ctx.y attrs binScale (toIndexed ctx.y 0 ctx.points.y) YDatum
                        , viewBars ctx ctx.z attrs binScale (toIndexed ctx.z yCount ctx.points.z) ZDatum
                        ]
                )
                |> W.Chart.Widget.withLabels
                    (\ctx ->
                        let
                            yCount : Int
                            yCount =
                                binCount ctx.y

                            zCount : Int
                            zCount =
                                binCount ctx.z

                            binScale : Scale.BandScale Int
                            binScale =
                                toBinScale attrs ctx (yCount + zCount)
                        in
                        W.Chart.Widget.Label.viewBinsList
                            [ attrs.labelPosition
                            , attrs.labelFormat
                            ]
                            { binScale = binScale
                            , ctx = ctx
                            , points =
                                ctx.points.byX
                                    |> Dict.values
                                    |> List.map
                                        (\data ->
                                            { x = data.x
                                            , y = data.y
                                            , z = data.z
                                            }
                                        )
                            }
                    )
                |> W.Chart.Widget.withHover
                    (\ctx ->
                        let
                            yCount : Int
                            yCount =
                                binCount ctx.y

                            zCount : Int
                            zCount =
                                binCount ctx.z

                            binScale : Scale.BandScale Int
                            binScale =
                                toBinScale attrs ctx (yCount + zCount)
                        in
                        \_ point ->
                            let
                                yItems : List ( Int, W.Chart.RenderDatum )
                                yItems =
                                    toIndexedMap .render ctx.y 0 point.y

                                zItems : List ( Int, W.Chart.RenderDatum )
                                zItems =
                                    toIndexedMap .render ctx.z yCount point.z
                            in
                            S.g []
                                [ viewHover ctx.y binScale point.x.render yItems
                                , viewHover ctx.z binScale point.x.render zItems
                                ]
                    )
        )



-- Helpers


viewHover : W.Chart.Internal.RenderAxisYZ a -> Scale.BandScale Int -> W.Chart.Internal.RenderDatum -> List ( Int, W.Chart.Internal.RenderDatum ) -> SC.Svg msg
viewHover axis binScale xPoint yzPoints =
    yzPoints
        |> List.filterMap
            (\( index, yzPoint ) ->
                let
                    height : Float
                    height =
                        abs (yzPoint.valueStart - yzPoint.valueEnd)

                    x : Float
                    x =
                        xPoint.valueStart + Scale.convert binScale index
                in
                if height > 0.0 then
                    Just
                        (viewBar
                            [ Svg.Attributes.stroke "white"
                            , SAP.strokeWidth 2
                            ]
                            { color = yzPoint.color
                            , x = x
                            , y = yzPoint.valueStart
                            , width = Scale.bandwidth binScale
                            , height = height
                            , axis = axis
                            }
                        )

                else
                    Nothing
            )
        |> S.g []


viewBars : W.Chart.Context x y z -> W.Chart.Internal.RenderAxisYZ a -> Attributes eventTarget msg -> Scale.BandScale Int -> List ( Int, W.Chart.Internal.AxisDataPoints x a ) -> (a -> eventTarget) -> SC.Svg msg
viewBars ctx axis attrs binScale indexedAxes toEventTarget =
    indexedAxes
        |> List.concatMap
            (\( index, ( axisDatum, points ) ) ->
                points
                    |> List.filterMap
                        (\( xPoint, yzPoint ) ->
                            let
                                height : Float
                                height =
                                    abs (yzPoint.render.valueStart - yzPoint.render.valueEnd)

                                x : Float
                                x =
                                    xPoint.render.valueStart + Scale.convert binScale index

                                eventTarget : eventTarget
                                eventTarget =
                                    toEventTarget yzPoint.datum.datum
                            in
                            if height > 0.0 then
                                Just
                                    (S.g
                                        [ W.Chart.Internal.attrAnimationDelayX ctx x
                                        , W.Chart.Internal.attrTransformOrigin x axis.zero
                                        , Svg.Attributes.class "ew-charts--animate-scale-z"
                                        ]
                                        [ viewBar
                                            [ attrs.onClick
                                                |> Maybe.map (\fn -> SE.onClick (fn eventTarget))
                                                |> Maybe.withDefault (Svg.Attributes.class "")
                                            , attrs.onMouseEnter
                                                |> Maybe.map (\fn -> SE.onMouseOver (fn eventTarget))
                                                |> Maybe.withDefault (Svg.Attributes.class "")
                                            , attrs.onMouseLeave
                                                |> Maybe.map (\fn -> SE.onMouseOut (fn eventTarget))
                                                |> Maybe.withDefault (Svg.Attributes.class "")
                                            ]
                                            { color = axisDatum.color
                                            , x = x
                                            , y = yzPoint.render.valueStart
                                            , width = Scale.bandwidth binScale
                                            , height = abs (yzPoint.render.valueStart - yzPoint.render.valueEnd)
                                            , axis = axis
                                            }
                                        ]
                                    )

                            else
                                Nothing
                        )
            )
        |> S.g []


viewBar :
    List (Svg.Attribute msg)
    ->
        { color : String
        , x : Float
        , y : Float
        , width : Float
        , height : Float
        , axis : W.Chart.Internal.RenderAxisYZ a
        }
    -> SC.Svg msg
viewBar attrs props =
    S.rect
        ([ Svg.Attributes.fill props.color
         , SAP.x props.x
         , SAP.width props.width
         , SAP.height props.height
         , if props.axis.isStacked || props.y < props.axis.zero then
            SAP.y props.y

           else
            SAP.y props.axis.zero
         ]
            ++ attrs
        )
        []


binCount : W.Chart.Internal.RenderAxisYZ a -> Int
binCount axis =
    case List.length axis.data of
        0 ->
            0

        length ->
            if axis.isStacked then
                1

            else
                length


toIndexedMap : (item -> a) -> W.Chart.Internal.RenderAxisYZ yz -> Int -> List item -> List ( Int, a )
toIndexedMap fn axis offset points =
    if axis.isStacked then
        List.map (\item -> ( offset, fn item )) points

    else
        List.indexedMap (\index item -> ( index + offset, fn item )) points


toIndexed : W.Chart.Internal.RenderAxisYZ yz -> Int -> List item -> List ( Int, item )
toIndexed =
    toIndexedMap identity


toBinScale : Attributes a msg -> W.Chart.Context x y z -> Int -> Scale.BandScale Int
toBinScale attrs ctx count =
    Scale.band
        { paddingInner = attrs.innerMargin
        , paddingOuter = attrs.outerMargin
        , align = 0.5
        }
        ( 0, Scale.bandwidth ctx.x.binScale )
        (List.range 0 (count - 1))

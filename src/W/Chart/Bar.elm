module W.Chart.Bar exposing
    ( fromY, fromZ, fromYZ
    , margins, Attribute
    , labelFormat, labelFormatWithList, labelsAsPercentages, labelsOutside
    )

{-|

@docs fromY, fromZ, fromYZ

@docs margins, Attribute

-}

import Attr
import Dict
import Scale
import Svg
import Svg.Attributes
import TypedSvg as S
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC
import W.Chart
import W.Chart.Internal
import W.Chart.Widget
import W.Chart.Widget.Label



-- Attributes


{-| -}
type alias Attribute =
    Attr.Attr Attributes


type alias Attributes =
    { outerMargin : Float
    , innerMargin : Float
    , labelPosition : W.Chart.Widget.Label.Attribute
    , labelFormat : W.Chart.Widget.Label.Attribute
    }


defaultAttrs : Attributes
defaultAttrs =
    { innerMargin = 0.2
    , outerMargin = 0.5
    , labelPosition = W.Chart.Widget.Label.inside
    , labelFormat = Attr.none
    }


{-| -}
margins : Float -> Float -> Attribute
margins innerMargin outerMargin =
    Attr.attr (\attr -> { attr | innerMargin = innerMargin, outerMargin = outerMargin })


{-| -}
labelsOutside : Attribute
labelsOutside =
    Attr.attr (\attr -> { attr | labelPosition = Attr.none })


{-| -}
labelFormat : (Float -> String) -> Attribute
labelFormat fn =
    Attr.attr (\attr -> { attr | labelFormat = W.Chart.Widget.Label.format fn })


{-| -}
labelFormatWithList : (List Float -> Float -> String) -> Attribute
labelFormatWithList fn =
    Attr.attr (\attr -> { attr | labelFormat = W.Chart.Widget.Label.formatWithList fn })


{-| -}
labelsAsPercentages : Attribute
labelsAsPercentages =
    Attr.attr (\attr -> { attr | labelFormat = W.Chart.Widget.Label.formatAsPercentage })



-- Views


{-| -}
fromY : List Attribute -> W.Chart.WidgetXY msg x y z a
fromY =
    Attr.withAttrs defaultAttrs
        (\attrs ->
            W.Chart.Widget.fromY
                (\ctx ->
                    viewBars
                        ctx
                        ctx.y
                        (toBinScale attrs ctx (binCount ctx.y))
                        (toIndexed ctx.y 0 ctx.points.y)
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
fromZ : List Attribute -> W.Chart.WidgetXYZ msg x y z a
fromZ =
    Attr.withAttrs defaultAttrs
        (\attrs ->
            W.Chart.Widget.fromZ
                (\ctx ->
                    viewBars
                        ctx
                        ctx.z
                        (toBinScale attrs ctx (binCount ctx.z))
                        (toIndexed ctx.z 0 ctx.points.z)
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
fromYZ : List Attribute -> W.Chart.WidgetXYZ msg x y z a
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
                        [ viewBars ctx ctx.y binScale (toIndexed ctx.y 0 ctx.points.y)
                        , viewBars ctx ctx.z binScale (toIndexed ctx.z yCount ctx.points.z)
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


viewBars : W.Chart.Context x y z -> W.Chart.Internal.RenderAxisYZ a -> Scale.BandScale Int -> List ( Int, W.Chart.Internal.AxisDataPoints x a ) -> SC.Svg msg
viewBars ctx axis binScale indexedAxes =
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
                            in
                            if height > 0.0 then
                                Just
                                    (S.g
                                        [ W.Chart.Internal.attrAnimationDelayX ctx x
                                        , W.Chart.Internal.attrTransformOrigin x axis.zero
                                        , Svg.Attributes.class "ew-charts--animate-scale-z"
                                        ]
                                        [ viewBar []
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


toBinScale : Attributes -> W.Chart.Context x y z -> Int -> Scale.BandScale Int
toBinScale attrs ctx count =
    Scale.band
        { paddingInner = attrs.innerMargin
        , paddingOuter = attrs.outerMargin
        , align = 0.5
        }
        ( 0, Scale.bandwidth ctx.x.binScale )
        (List.range 0 (count - 1))

module W.Chart.Widget.Label exposing (Attribute, format, formatAsPercentage, formatWithList, inside, view, viewBinsList, viewList)

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
    }


defaultAttrs : Attributes
defaultAttrs =
    { position = Outside
    , format = Nothing
    }


type Position
    = Outside
    | Inside


{-| -}
inside : Attribute
inside =
    Attr.attr (\attr -> { attr | position = Inside })


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
    , alignBottom : Bool
    }
    -> SC.Svg msg
view props =
    S.text_
        [ SAP.x props.x
        , SAP.strokeWidth 3
        , Svg.Attributes.fill Theme.baseForeground
        , Svg.Attributes.stroke Theme.baseBackground
        , Svg.Attributes.style "paint-order:stroke"
        , SAP.fontSize props.ctx.fontSize.lg
        , SA.textAnchor ST.AnchorMiddle
        , if props.alignBottom then
            SAP.y (props.y + props.ctx.fontSize.lg * 1.1)

          else
            SAP.y (props.y - props.ctx.fontSize.lg * 0.5)
        ]
        [ SC.text props.label
        ]


{-| -}
viewList :
    List Attribute
    -> { ctx : W.Chart.Context x y z, points : List ( W.Chart.RenderDatum, List W.Chart.RenderDatum ) }
    -> SC.Svg msg
viewList =
    Attr.withAttrs defaultAttrs
        (\attrs props ->
            props.points
                |> List.concatMap
                    (\( x, ys ) ->
                        ys
                            |> List.map
                                (\y -> viewPoint attrs props.ctx x.valueScaled ys y False)
                    )
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
            let
                yCount : Int
                yCount =
                    binCount props.ctx.y
            in
            props.points
                |> List.concatMap
                    (\point ->
                        viewBinsListPoint attrs
                            { binScale = props.binScale
                            , ctx = props.ctx
                            , x = point.x
                            , yz = point.y
                            , offset = 0
                            , isStacked = props.ctx.y.isStacked
                            }
                            ++ viewBinsListPoint attrs
                                { binScale = props.binScale
                                , ctx = props.ctx
                                , x = point.x
                                , yz = point.z
                                , offset = yCount
                                , isStacked = props.ctx.z.isStacked
                                }
                    )
                |> S.g []
        )


viewBinsListPoint :
    Attributes
    ->
        { binScale : Scale.BandScale Int
        , ctx : W.Chart.Context x y z
        , x : W.Chart.Point x
        , yz : List (W.Chart.Point a)
        , offset : Int
        , isStacked : Bool
        }
    -> List (SC.Svg msg)
viewBinsListPoint attrs props =
    props.yz
        |> List.indexedMap
            (\index point ->
                let
                    x : Float
                    x =
                        if props.isStacked then
                            props.x.render.valueStart
                                + (Scale.bandwidth props.binScale * 0.5)
                                + Scale.convert props.binScale props.offset

                        else
                            props.x.render.valueStart
                                + (Scale.bandwidth props.binScale * 0.5)
                                + Scale.convert props.binScale (index + props.offset)
                in
                viewPoint
                    attrs
                    props.ctx
                    x
                    (List.map .render props.yz)
                    point.render
                    props.isStacked
            )


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


viewPoint :
    Attributes
    -> W.Chart.Context x y z
    -> Float
    -> List W.Chart.RenderDatum
    -> W.Chart.RenderDatum
    -> Bool
    -> SC.Svg msg
viewPoint attrs ctx x pointList point isStacked =
    if point.value == 0.0 then
        H.text ""

    else
        let
            position : Position
            position =
                if isStacked then
                    Inside

                else
                    attrs.position
        in
        view
            { ctx = ctx
            , x = x
            , y = point.valueScaled
            , color = point.color
            , alignBottom = (point.value > 0.0 && position == Inside) || (point.value <= 0.0 && position == Outside)
            , label =
                attrs.format
                    |> Maybe.map (\fn -> fn (List.map .value pointList) point.value)
                    |> Maybe.withDefault point.valueString
            }



-- S.text_
-- [ SAP.x x
-- , SAP.strokeWidth 3
-- , Svg.Attributes.fill point.color
-- , Svg.Attributes.stroke Theme.baseBackground
-- , Svg.Attributes.style "paint-order:stroke"
-- , SAP.fontSize ctx.fontSize.lg
-- , SA.textAnchor ST.AnchorMiddle
-- , if
--     (point.value > 0.0 && (attrs.position == Inside || isStacked))
--         || (attrs.position == Outside && point.value < 0.0)
--   then
--     SAP.y (point.valueScaled + ctx.fontSize.lg * 1.1)
--   else
--     SAP.y (point.valueScaled - ctx.fontSize.lg * 0.5)
-- ]
-- [ attrs.format
--     |> Maybe.map (\fn -> fn (List.map .value pointList) point.value)
--     |> Maybe.withDefault point.valueString
--     |> SC.text
-- ]

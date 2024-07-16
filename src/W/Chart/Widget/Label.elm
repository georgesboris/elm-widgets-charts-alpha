module W.Chart.Widget.Label exposing (view, viewBinsList, viewList)

import Svg.Attributes
import Theme
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC
import TypedSvg.Types as ST
import W.Chart
import W.Chart.Internal


{-| -}
view :
    W.Chart.Context x y z
    ->
        { x : Float
        , y : Float
        , alignBottom : Bool
        , color : String
        , label : String
        }
    -> SC.Svg msg
view ctx props =
    S.text_
        [ SAP.x props.x
        , SAP.strokeWidth 3
        , Svg.Attributes.fill props.color
        , Svg.Attributes.stroke Theme.baseBackground
        , Svg.Attributes.style "paint-order:stroke"
        , SAP.fontSize ctx.fontSize.md
        , SA.textAnchor ST.AnchorMiddle
        , if props.alignBottom then
            SAP.y (props.y + ctx.fontSize.md * 1.2)

          else
            SAP.y (props.y - ctx.fontSize.md * 0.5)
        ]
        [ SC.text props.label
        ]


{-| -}
viewList :
    W.Chart.Context x y z
    -> List (W.Chart.Internal.AxisDataPoints x a)
    -> SC.Svg msg
viewList ctx axisPoints =
    axisPoints
        |> List.concatMap
            (\( yDatum, yDataPoints ) ->
                yDataPoints
                    |> List.filterMap
                        (\( x, y ) ->
                            if y.render.value == 0.0 then
                                Nothing

                            else
                                Just
                                    (view ctx
                                        { x = x.render.valueScaled
                                        , y = y.render.valueScaled
                                        , color = yDatum.color
                                        , label = y.render.valueString
                                        , alignBottom = y.render.value < 0.0
                                        }
                                    )
                        )
            )
        |> S.g []


{-| -}
viewBinsList :
    W.Chart.Context x y z
    -> List (W.Chart.Internal.AxisDataPoints x a)
    -> SC.Svg msg
viewBinsList ctx axisPoints =
    axisPoints
        |> List.concatMap
            (\( yDatum, yDataPoints ) ->
                yDataPoints
                    |> List.filterMap
                        (\( x, y ) ->
                            if y.render.value == 0.0 then
                                Nothing

                            else
                                Just
                                    (view ctx
                                        { x = x.render.valueScaled
                                        , y = y.render.valueScaled
                                        , color = yDatum.color
                                        , label = y.render.valueString
                                        , alignBottom = y.render.value > 0.0
                                        }
                                    )
                        )
            )
        |> S.g []

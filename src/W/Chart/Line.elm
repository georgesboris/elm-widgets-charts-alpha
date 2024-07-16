module W.Chart.Line exposing
    ( fromY, fromZ
    , smooth, dashed, areaAlways, lineAlways, Attribute
    )

{-|

@docs fromY, fromZ

@docs smooth, dashed, areaAlways, lineAlways, Attribute

-}

import Attr
import Path
import Shape
import SubPath
import Svg.Attributes
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
fromY : List Attribute -> W.Chart.WidgetXY msg x y z a
fromY =
    Attr.withAttrs defaultAttrs
        (\attrs ->
            W.Chart.Widget.fromY (\ctx -> viewLines attrs ctx.y ctx.points.y)
                |> W.Chart.Widget.withLabels (\ctx _ _ -> W.Chart.Widget.Label.viewList)
                |> W.Chart.Widget.withHover (\_ _ point -> viewHover point.x point.y)
        )


{-| -}
fromZ : List Attribute -> W.Chart.WidgetXYZ msg x y z a
fromZ =
    Attr.withAttrs defaultAttrs
        (\attrs ->
            W.Chart.Widget.fromZ (\ctx -> viewLines attrs ctx.z ctx.points.z)
                |> W.Chart.Widget.withLabels (\ctx _ point -> W.Chart.Widget.Label.viewList ctx point.x point.z)
                |> W.Chart.Widget.withHover (\_ _ point -> viewHover point.x point.z)
        )



-- Attributes


{-| -}
type alias Attribute =
    Attr.Attr Attributes


type alias Attributes =
    { smooth : Bool
    , dashed : Bool
    , area : Maybe Bool
    }


defaultAttrs : Attributes
defaultAttrs =
    { smooth = False
    , dashed = False
    , area = Nothing
    }


{-| -}
smooth : Attribute
smooth =
    Attr.attr (\attr -> { attr | smooth = True })


{-| -}
dashed : Attribute
dashed =
    Attr.attr (\attr -> { attr | dashed = True })


{-| -}
lineAlways : Attribute
lineAlways =
    Attr.attr (\attr -> { attr | area = Just False })


{-| -}
areaAlways : Attribute
areaAlways =
    Attr.attr (\attr -> { attr | area = Just True })



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


viewLines : Attributes -> W.Chart.Internal.RenderAxisYZ a -> List ( W.Chart.Internal.ChartDatum a, List ( W.Chart.Internal.DataPoint x, W.Chart.Internal.DataPoint a ) ) -> SC.Svg msg
viewLines attrs axis axisPoints =
    let
        viewLine_ : Attributes -> Int -> ( W.Chart.Internal.ChartDatum a, List ( W.Chart.Internal.DataPoint x, W.Chart.Internal.DataPoint a ) ) -> SC.Svg msg
        viewLine_ =
            case attrs.area of
                Just True ->
                    viewLineWithArea axis

                Just False ->
                    viewLine

                Nothing ->
                    if axis.isStacked || (List.length axis.data == 1) then
                        viewLineWithArea axis

                    else
                        viewLine
    in
    S.g [] (List.indexedMap (viewLine_ attrs) axisPoints)


viewLineWithArea : W.Chart.Internal.RenderAxisYZ a -> Attributes -> Int -> ( W.Chart.Internal.ChartDatum a, List ( W.Chart.Internal.DataPoint x, W.Chart.Internal.DataPoint a ) ) -> SC.Svg msg
viewLineWithArea _ attrs index ( chartDatum, points ) =
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
            List.map (Maybe.map Tuple.first) areaPoints

        gradientId : String
        gradientId =
            chartDatum.color
                |> String.replace "." "_"
                |> String.replace "," "_"
                |> String.replace "(" "_"
                |> String.replace ")" "_"
                |> String.replace "#" "_"
                |> (++) "ew-charts--g-"
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
            [ Svg.Attributes.class "ew-charts--animate-fade"
            , Svg.Attributes.style ("animation-delay:" ++ String.fromInt (index * 400))
            , Svg.Attributes.fill ("url(#" ++ gradientId ++ ")")
            ]
        , Path.element
            (Shape.line (shape attrs) linePoints)
            [ Svg.Attributes.class "ew-charts--animate-h-clip"
            , Svg.Attributes.style ("animation-delay:" ++ String.fromInt (index * 400))
            , Svg.Attributes.fill "transparent"
            , Svg.Attributes.strokeWidth "2px"
            , Svg.Attributes.stroke chartDatum.color
            , if attrs.dashed then
                SA.strokeDasharray "4 4"

              else
                Svg.Attributes.class ""
            ]
        ]


viewLine : Attributes -> Int -> ( W.Chart.Internal.ChartDatum a, List ( W.Chart.Internal.DataPoint x, W.Chart.Internal.DataPoint a ) ) -> SC.Svg msg
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
        [ Svg.Attributes.class "ew-charts--animate-h-clip"
        , Svg.Attributes.style ("animation-delay:" ++ String.fromInt (index * 400))
        , Svg.Attributes.fill "transparent"
        , Svg.Attributes.strokeWidth "2px"
        , Svg.Attributes.stroke chartDatum.color
        , if attrs.dashed then
            SA.strokeDasharray "4 4"

          else
            Svg.Attributes.class ""
        ]


shape : Attributes -> List ( Float, Float ) -> SubPath.SubPath
shape attrs =
    if attrs.smooth then
        Shape.monotoneInXCurve

    else
        Shape.linearCurve

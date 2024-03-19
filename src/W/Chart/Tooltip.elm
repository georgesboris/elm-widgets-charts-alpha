module W.Chart.Tooltip exposing
    ( fromY, fromZ, fromYZ
    , formatByX, header, Attribute
    )

{-|

@docs fromY, fromZ, fromYZ

@docs formatByX, header, Attribute

-}

import Attr
import Html as H
import Html.Attributes as HA
import Scale
import Svg
import Svg.Attributes
import Theme
import TypedSvg.Attributes as SA
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC
import W.Chart
import W.Chart.Internal
import W.Chart.Widget


{-| -}
fromY : List (Attribute msg x y z point) -> W.Chart.WidgetXY msg x y z a
fromY =
    Attr.withAttrs defaultAttrs
        (\attrs ->
            W.Chart.Widget.empty
                |> W.Chart.Widget.withHover
                    (\ctx coords point ->
                        if List.isEmpty point.y then
                            H.text ""

                        else
                            view ctx
                                coords
                                [ viewX point.x
                                , viewYZ attrs ctx ctx.y point.y
                                ]
                    )
        )


{-| -}
fromZ : List (Attribute msg x y z point) -> W.Chart.WidgetXYZ msg x y z a
fromZ =
    Attr.withAttrs defaultAttrs
        (\attrs ->
            W.Chart.Widget.empty
                |> W.Chart.Widget.withHover
                    (\ctx coords point ->
                        if List.isEmpty point.z then
                            H.text ""

                        else
                            view ctx
                                coords
                                [ viewX point.x
                                , viewYZ attrs ctx ctx.z point.z
                                ]
                    )
        )


{-| -}
fromYZ : List (Attribute msg x y z point) -> W.Chart.WidgetXYZ msg x y z a
fromYZ =
    Attr.withAttrs defaultAttrs
        (\attrs ->
            W.Chart.Widget.empty
                |> W.Chart.Widget.withHover
                    (\ctx coords point ->
                        if List.isEmpty point.y && List.isEmpty point.z then
                            H.text ""

                        else
                            view ctx
                                coords
                                [ viewX point.x
                                , viewYZ attrs ctx ctx.y point.y
                                , viewYZ attrs ctx ctx.z point.z
                                ]
                    )
        )



-- Attributes


{-| -}
type alias Attribute msg x y z point =
    Attr.Attr (Attributes msg x y z point)


type alias Attributes msg x y z point =
    { format : Maybe (W.Chart.Context x y z -> List W.Chart.RenderDatum -> W.Chart.RenderDatum -> List (H.Html msg))
    , header : Maybe (W.Chart.Context x y z -> List W.Chart.RenderDatum -> List (H.Html msg))
    , other : Maybe point
    }


defaultAttrs : Attributes msg x y z point
defaultAttrs =
    { format = Nothing
    , header = Nothing
    , other = Nothing
    }


{-| -}
formatByX : (W.Chart.Context x y z -> List W.Chart.RenderDatum -> W.Chart.RenderDatum -> List (H.Html msg)) -> Attribute msg x y z point
formatByX v =
    Attr.attr (\attr -> { attr | format = Just v })


{-| -}
header : (W.Chart.Context x y z -> List W.Chart.RenderDatum -> List (H.Html msg)) -> Attribute msg x y z point
header v =
    Attr.attr (\attr -> { attr | header = Just v })



-- View


viewX : W.Chart.Point x -> H.Html msg
viewX x =
    H.h1
        [ HA.class "ew-charts--tooltip-x" ]
        [ H.text x.render.label
        ]


viewYZ :
    Attributes msg x y z point
    -> W.Chart.Internal.Context x y z
    -> W.Chart.Internal.RenderAxisYZ a
    -> List (W.Chart.Internal.DataPoint a)
    -> H.Html msg
viewYZ attrs ctx axisAttrs dataPoints =
    dataPoints
        |> W.Chart.Internal.maybeIf (not << List.isEmpty)
        |> Maybe.map
            (\_ ->
                let
                    orderedDataPoints : List (W.Chart.Internal.DataPoint a)
                    orderedDataPoints =
                        if axisAttrs.isStacked then
                            List.reverse dataPoints

                        else
                            dataPoints
                in
                H.section
                    [ HA.class "ew-charts--tooltip-yz" ]
                    [ viewAxisHeader attrs ctx axisAttrs dataPoints
                    , H.ul [ HA.class "ew-charts--tooltip-yz--list" ]
                        (List.map (viewItem attrs ctx dataPoints) orderedDataPoints)
                    ]
            )
        |> Maybe.withDefault (H.text "")


viewAxisHeader :
    Attributes msg x y z point
    -> W.Chart.Internal.Context x y z
    -> W.Chart.Internal.RenderAxisYZ a
    -> List (W.Chart.Internal.DataPoint a)
    -> H.Html msg
viewAxisHeader attrs ctx axisAttrs dataPoints =
    case ( axisAttrs.label, attrs.header ) of
        ( Just label, Just header_ ) ->
            H.h2 [ HA.class "ew-charts--tooltip-yz--label" ]
                [ H.span [] [ H.text label ]
                , H.span [] (header_ ctx (List.map .render dataPoints))
                ]

        ( Just label, Nothing ) ->
            H.h2 [ HA.class "ew-charts--tooltip-yz--label" ] [ H.text label ]

        ( Nothing, Just header_ ) ->
            H.h2 [ HA.class "ew-charts--tooltip-yz--label" ]
                [ H.span [] []
                , H.span [] (header_ ctx (List.map .render dataPoints))
                ]

        ( Nothing, Nothing ) ->
            H.text ""


viewItem : Attributes msg x y z point -> W.Chart.Context x y z -> List (W.Chart.Internal.DataPoint a) -> W.Chart.Internal.DataPoint a -> H.Html msg
viewItem attrs ctx pointsByX point =
    H.li
        [ HA.class "ew-charts--tooltip-yz--item" ]
        [ H.span
            [ HA.class "ew-charts--tooltip-yz--item-color"
            , HA.style "background" point.render.color
            ]
            []
        , H.span [ HA.class "ew-charts--tooltip-yz--item-label" ] [ H.text point.render.label ]
        , H.span
            [ HA.class "ew-charts--tooltip-yz--item-value" ]
            (case attrs.format of
                Just format_ ->
                    format_ ctx (List.map .render pointsByX) point.render

                Nothing ->
                    [ H.text point.render.valueString ]
            )
        ]


view :
    W.Chart.Context x y z
    -> W.Chart.Coordinates
    -> List (H.Html msg)
    -> SC.Svg msg
view ctx coords children =
    let
        x : Float
        x =
            coords.x

        y : Float
        y =
            Maybe.withDefault 0.0 coords.y

        margin : Float
        margin =
            Scale.bandwidth ctx.x.binScale

        alignTop : Bool
        alignTop =
            y <= (ctx.height * 0.5)

        alignLeft : Bool
        alignLeft =
            x >= (ctx.width * 0.5)

        wrapperX : Float
        wrapperX =
            if alignLeft then
                0

            else
                x + margin

        wrapperY : Float
        wrapperY =
            if alignTop then
                y

            else
                0

        wrapperWidth : Float
        wrapperWidth =
            if alignLeft then
                x - margin

            else
                ctx.width - x - margin

        wrapperHeight : Float
        wrapperHeight =
            if alignTop then
                ctx.height - y

            else
                y
    in
    Svg.foreignObject
        [ SA.class [ "ew-charts--tooltip-wrapper" ]
        , Svg.Attributes.width (String.fromFloat wrapperWidth ++ "px")
        , Svg.Attributes.height (String.fromFloat wrapperHeight ++ "px")
        , SAP.y wrapperY
        , SAP.x wrapperX
        ]
        [ H.div
            [ HA.attribute "xlmns" "http://www.w3.org/1999/xhtml"
            , HA.class "ew-charts--tooltip"
            , HA.classList
                [ ( "m--align-left", alignLeft )
                , ( "m--align-top", alignTop )
                ]
            , HA.style "width" (String.fromFloat wrapperWidth ++ "px")
            , HA.style "height" (String.fromFloat wrapperHeight ++ "px")
            ]
            [ H.div
                [ HA.class "ew-bg-base-bg ew-text-base-fg"
                , HA.class "ew-border ew-border-solid"
                , HA.class "ew-rounded ew-shadow"
                , Theme.styles
                    [ ( "border-color", Theme.baseForegroundWithAlpha 0.1 )
                    ]
                ]
                children
            ]
        ]

module W.Chart.Tooltip exposing
    ( fromY, fromZ, fromYZ
    , formatByX, headerValue, yAxisLabel, zAxisLabel, axisValue, Attribute
    , viewCustom
    )

{-|

@docs fromY, fromZ, fromYZ

@docs formatByX, headerValue, yAxisLabel, zAxisLabel, axisValue, Attribute

@docs viewCustom

-}

import Attr
import Html as H
import Html.Attributes as HA
import Scale
import Svg
import Svg.Attributes
import TypedSvg.Attributes as SA
import TypedSvg.Attributes.InPx as SAP
import TypedSvg.Core as SC
import W.Chart
import W.Chart.Internal
import W.Chart.Widget
import W.Theme
import W.Theme.Color


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
                                [ viewX attrs ctx point.x (List.map .render point.y)
                                , viewYZ attrs attrs.yConfig ctx ctx.y point.y
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
                                [ viewX attrs ctx point.x (List.map .render point.z)
                                , viewYZ attrs attrs.zConfig ctx ctx.z point.z
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
                                [ viewX attrs
                                    ctx
                                    point.x
                                    (List.map .render point.y ++ List.map .render point.z)
                                , viewYZ attrs attrs.yConfig ctx ctx.y point.y
                                , viewYZ attrs attrs.zConfig ctx ctx.z point.z
                                ]
                    )
        )



-- Attributes


{-| -}
type alias Attribute msg x y z point =
    Attr.Attr (Attributes msg x y z point)


type alias Attributes msg x y z point =
    { format : Maybe (W.Chart.Context x y z -> List W.Chart.RenderDatum -> W.Chart.RenderDatum -> List (H.Html msg))
    , headerValue : Maybe (W.Chart.Context x y z -> List W.Chart.RenderDatum -> List (H.Html msg))
    , yConfig : AxisConfig msg x y z
    , zConfig : AxisConfig msg x y z
    , other : Maybe point
    }


type alias AxisConfig msg x y z =
    { axisLabel : Maybe (List (H.Html msg))
    , axisValue : Maybe (W.Chart.Context x y z -> List W.Chart.RenderDatum -> List (H.Html msg))
    }


defaultAttrs : Attributes msg x y z point
defaultAttrs =
    { format = Nothing
    , headerValue = Nothing
    , yConfig = { axisLabel = Nothing, axisValue = Nothing }
    , zConfig = { axisLabel = Nothing, axisValue = Nothing }
    , other = Nothing
    }


{-| -}
formatByX : (W.Chart.Context x y z -> List W.Chart.RenderDatum -> W.Chart.RenderDatum -> List (H.Html msg)) -> Attribute msg x y z point
formatByX v =
    Attr.attr (\attr -> { attr | format = Just v })


{-| -}
headerValue : (W.Chart.Context x y z -> List W.Chart.RenderDatum -> List (H.Html msg)) -> Attribute msg x y z point
headerValue v =
    Attr.attr (\attr -> { attr | headerValue = Just v })


{-| -}
axisValue : (W.Chart.Context x y z -> List W.Chart.RenderDatum -> List (H.Html msg)) -> Attribute msg x y z point
axisValue v =
    Attr.attr
        (\attr ->
            let
                yConfig : AxisConfig msg x y z
                yConfig =
                    attr.yConfig

                zConfig : AxisConfig msg x y z
                zConfig =
                    attr.zConfig
            in
            { attr | yConfig = { yConfig | axisValue = Just v }, zConfig = { zConfig | axisValue = Just v } }
        )


{-| -}
yAxisLabel : List (H.Html msg) -> Attribute msg x y z point
yAxisLabel v =
    Attr.attr
        (\attr ->
            let
                yConfig : AxisConfig msg x y z
                yConfig =
                    attr.yConfig
            in
            { attr | yConfig = { yConfig | axisLabel = Just v } }
        )


{-| -}
zAxisLabel : List (H.Html msg) -> Attribute msg x y z point
zAxisLabel v =
    Attr.attr
        (\attr ->
            let
                zConfig : AxisConfig msg x y z
                zConfig =
                    attr.zConfig
            in
            { attr | zConfig = { zConfig | axisLabel = Just v } }
        )



-- View


{-| -}
viewCustom :
    { x : Float
    , y : Float
    , content : List (H.Html msg)
    }
    -> H.Html msg
viewCustom props =
    Svg.foreignObject
        [ SA.class [ "ew-charts--tooltip-wrapper" ]
        , SAP.x props.x
        , SAP.y props.y
        ]
        [ H.div [ HA.attribute "xlmns" "http://www.w3.org/1999/xhtml" ]
            props.content
        ]


viewX :
    Attributes msg x y z point
    -> W.Chart.Internal.Context x y z
    -> W.Chart.Point x
    -> List W.Chart.RenderDatum
    -> H.Html msg
viewX attrs ctx x yzPoints =
    H.h1
        [ HA.class "ew-charts--tooltip-x" ]
        [ H.span [ HA.class "ew-charts--tooltip-x--label" ] [ H.text x.render.label ]
        , attrs.headerValue
            |> Maybe.map
                (\fn ->
                    H.span
                        [ HA.class "ew-charts--tooltip-x--value " ]
                        (fn ctx yzPoints)
                )
            |> Maybe.withDefault (H.text "")
        ]


viewYZ :
    Attributes msg x y z point
    -> AxisConfig msg x y z
    -> W.Chart.Internal.Context x y z
    -> W.Chart.Internal.RenderAxisYZ a
    -> List (W.Chart.Internal.DataPoint a)
    -> H.Html msg
viewYZ attrs axis ctx axisAttrs dataPoints =
    dataPoints
        |> W.Chart.Internal.maybeIf (not << List.isEmpty)
        |> Maybe.map
            (\_ ->
                H.section
                    [ HA.class "ew-charts--tooltip-yz" ]
                    [ viewAxisHeader axis ctx axisAttrs dataPoints
                    , H.ul [ HA.class "ew-charts--tooltip-yz--list" ]
                        (List.map (viewItem attrs ctx dataPoints) dataPoints)
                    ]
            )
        |> Maybe.withDefault (H.text "")


viewAxisHeader :
    AxisConfig msg x y z
    -> W.Chart.Internal.Context x y z
    -> W.Chart.Internal.RenderAxisYZ a
    -> List (W.Chart.Internal.DataPoint a)
    -> H.Html msg
viewAxisHeader attrs ctx axisAttrs dataPoints =
    if axisAttrs.label == Nothing && attrs.axisLabel == Nothing && attrs.axisValue == Nothing then
        H.text ""

    else
        let
            axisLabel_ : H.Html msg
            axisLabel_ =
                case attrs.axisLabel of
                    Just label ->
                        H.span [] label

                    Nothing ->
                        H.span [] [ H.text (Maybe.withDefault "" axisAttrs.label) ]

            axisValue_ : H.Html msg
            axisValue_ =
                attrs.axisValue
                    |> Maybe.map (\fn -> H.span [] (fn ctx (List.map .render dataPoints)))
                    |> Maybe.withDefault (H.text "")
        in
        H.h2 [ HA.class "ew-charts--tooltip-yz--label" ]
            [ axisLabel_
            , axisValue_
            ]


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
            Scale.bandwidth ctx.x.binScale * 0.25

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
                [ HA.class "w/base"
                , HA.class "w--bg w--text-default w--text-subtle"
                , HA.class "w--border w--border-solid"
                , HA.class "w--rounded w--shadow-shadow w--shadow"
                , W.Theme.styleList
                    [ ( "border-color", W.Theme.Color.tintSubtle )
                    ]
                ]
                [ H.div
                    [ HA.style "background" "rgba(0,0,0,0.2)"
                    , HA.class "w--rounded"
                    ]
                    children
                ]
            ]
        ]

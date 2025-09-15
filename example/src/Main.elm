module Main exposing (main)

import Browser
import Date exposing (Date)
import FormatNumber
import FormatNumber.Locales
import Html as H
import Html.Attributes as HA
import Time exposing (Month(..))
import W.Chart
import W.Chart.Bar
import W.Chart.Line
import W.Chart.Colors
import W.Chart.Tooltip
import W.Styles
import W.Theme
import W.Theme.Color


type alias Model =
    { onClickDatum : Maybe String
    , onClick : Maybe ( W.Chart.Coordinates, String )
    , onHover : Maybe ( W.Chart.Coordinates, String )
    }


type Msg
    = OnClick W.Chart.Coordinates String
    | OnClickDatum String
    | OnMouseEnter W.Chart.Coordinates String
    | OnMouseLeave
    | OnMouseLeaveChart


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnClickDatum color ->
            { model | onClickDatum = Just color }

        OnClick coords color ->
            { model | onClick = Just ( coords, color ) }

        OnMouseEnter coords color ->
            { model | onHover = Just ( coords, color ) }

        OnMouseLeave ->
            { model | onHover = Nothing }

        OnMouseLeaveChart ->
            { model | onHover = Nothing, onClick = Nothing }



-- 2. Exploring visualizations


main : Program () Model Msg
main =
    Browser.sandbox
        { init = { onClickDatum = Nothing, onClick = Nothing, onHover = Nothing }
        , update = update
        , view =
            \model ->
                viewWrapper model
                    [ W.Chart.fromXYZ
                        [ W.Chart.dontAutoHideLabels
                        , W.Chart.topLegends
                        , W.Chart.legendPadding 10
                        , W.Chart.annotationsPadding 30
                        , W.Chart.header [ H.h1 [ HA.style "color" "white" ] [ H.text "My Chart" ] ]
                        , W.Chart.footer [ H.p [ HA.style "color" "white" ] [ H.text "My Chart" ] ]
                        , W.Chart.paddingLeft 80
                        , W.Chart.onMouseLeaveChart OnMouseLeaveChart
                        ]
                        { x =
                            W.Chart.xAxis
                                [ W.Chart.ticks 6
                                , W.Chart.axisLabel "X Axis"
                                , W.Chart.axisLabelPadding 60
                                ]
                                { data =
                                    List.range 1 5
                                        |> List.map (\i -> Date.fromOrdinalDate 2023 i)
                                , toLabel = Date.format "MMM d"
                                }
                        , y =
                            W.Chart.axisList
                                [ W.Chart.axisLabel "Y Axis"
                                , W.Chart.axisLabelPadding 60
                                , W.Chart.stacked
                                , W.Chart.formatStack
                                    (\xs ->
                                        xs
                                            |> List.sum
                                            |> formatFloat
                                    )
                                ]
                                { data = List.range 0 3
                                , toLabel = String.fromInt
                                , toValue = toValue (\x -> Basics.sin x)
                                , toColor = W.Chart.Colors.colorByIndex W.Chart.Colors.warm
                                }
                        , z =
                            W.Chart.axisList
                                [ W.Chart.axisLabel "Z Axis"
                                , W.Chart.stacked
                                , W.Chart.formatStack
                                    (\xs ->
                                        xs
                                            |> List.sum
                                            |> formatFloat
                                    )
                                ]
                                { data = List.range 4 6
                                , toLabel = String.fromInt
                                , toValue = toValue Basics.cos
                                , toColor = W.Chart.Colors.colorByIndex W.Chart.Colors.cool
                                }
                        }
                        |> W.Chart.withActive (Maybe.map Tuple.first model.onClick)
                        |> W.Chart.withHover
                            [ W.Chart.onMouseEnter (\c a -> OnMouseEnter c (toColor a))
                            , W.Chart.onMouseLeave (\_ _ -> OnMouseLeave)
                            ]
                        |> W.Chart.view
                            [ 
                            --   W.Chart.Line.fromZ []
                            -- , W.Chart.Bar.fromY [ W.Chart.Bar.showLabels ]
                             W.Chart.Bar.fromYZ [ W.Chart.Bar.showLabels, W.Chart.Bar.labelsAsPercentages ]
                            , W.Chart.Tooltip.fromYZ
                                [ W.Chart.Tooltip.yAxisLabel [ H.text "YYY" ]
                                , W.Chart.Tooltip.headerValue
                                    (\ctx yList ->
                                        sumAt .value yList
                                            |> ctx.y.format
                                            |> H.text
                                            |> List.singleton
                                    )
                                , W.Chart.Tooltip.formatByX
                                    (\_ yList y ->
                                        let
                                            total : Float
                                            total =
                                                sumAt .value yList

                                            pct : Float
                                            pct =
                                                if total /= 0 then
                                                    y.value / total

                                                else
                                                    0
                                        in
                                        [ H.text y.valueString
                                        , H.text " "
                                        , H.span
                                            [ HA.style "color" "gray"
                                            , HA.style "font-size" "0.8em"
                                            ]
                                            [ H.text ("(" ++ pctString pct ++ ")") ]
                                        ]
                                    )
                                ]
                            ]
                    ]
        }


toValue : (Float -> Float) -> Int -> Date -> Maybe Float
toValue fn a x =
    Just (fn (toFloat (a + Date.ordinalDay x)))



--


viewWrapper : Model -> List (H.Html msg) -> H.Html msg
viewWrapper model children =
    H.div
        [ HA.style "background" W.Theme.Color.baseBg
        , HA.style "min-height" "100vh"
        , HA.style "padding" "20px"
        , HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        ]
        [ W.Chart.globalStyles
        , W.Styles.view [ W.Styles.darkTheme ]
        , globalStyles
        , viewColor ( 20, 20 ) (Maybe.map Tuple.second model.onClick)
        , viewColor ( 80, 20 ) (Maybe.map Tuple.second model.onHover)
        , viewColor ( 140, 20 ) model.onClickDatum
        , children
            |> List.map
                (\c ->
                    H.div
                        [ HA.style "background" W.Theme.Color.baseBg
                        , HA.style "border" ("1px solid " ++ W.Theme.Color.baseAccent)
                        , HA.style "border-radius" "8px"
                        , HA.style "box-shadow" "0 0 20px rgba(0,0,0,0.2)"
                        ]
                        [ c ]
                )
            |> H.div
                [ HA.style "width" "100%"
                , HA.style "max-width" "960px"
                , HA.style "display" "flex"
                , HA.style "flex-direction" "column"
                , HA.style "gap" "20px"
                ]
        ]


globalStyles : H.Html msg
globalStyles =
    H.node "style"
        []
        [ H.text """

.w__charts--tooltip {
    font-family: var(--theme-font-text), sans-serif;
}
"""
        ]


viewColor : ( Float, Float ) -> Maybe String -> H.Html msg
viewColor ( top, right ) maybeColor =
    maybeColor
        |> Maybe.map
            (\color ->
                H.div
                    [ HA.style "width" "40px"
                    , HA.style "height" "40px"
                    , HA.style "border-radius" "20px"
                    , HA.style "position" "fixed"
                    , HA.style "background" color
                    , HA.style "top" (String.fromFloat top ++ "px")
                    , HA.style "right" (String.fromFloat right ++ "px")
                    ]
                    []
            )
        |> Maybe.withDefault (H.text "")


toColor : { a | y : List (W.Chart.Point y) } -> String
toColor point =
    point.y
        |> List.head
        |> Maybe.map (.render >> .color)
        |> Maybe.withDefault "black"



--


sumAt : (a -> Float) -> List a -> Float
sumAt fn xs =
    List.foldl (\x acc -> fn x + acc) 0 xs


baseLocale : FormatNumber.Locales.Locale
baseLocale =
    FormatNumber.Locales.base


formatFloat : Float -> String
formatFloat =
    FormatNumber.format { baseLocale | decimals = FormatNumber.Locales.Exact 2 }


pctString : Float -> String
pctString v =
    (v * 100)
        |> formatFloat
        |> (\x -> x ++ "%")

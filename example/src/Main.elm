module Main exposing (main)

import Browser
import Color
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
                        [ W.Chart.width 960
                        , W.Chart.dontAutoHideLabels
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
                                    List.range 1 8
                                        |> List.map (\i -> Date.fromOrdinalDate 2023 i)
                                , toLabel = Date.format "MMM d"
                                }
                        , y =
                            W.Chart.axisList
                                [ W.Chart.axisLabel "Y Axis"
                                , W.Chart.axisLabelPadding 60
                                -- , W.Chart.stacked
                                , W.Chart.format (formatDecimals 4)
                                , W.Chart.formatStack
                                    (\xs ->
                                        xs
                                            |> List.sum
                                            |> formatFloat
                                    )
                                ]
                                { data = List.range 0 6
                                , toLabel = String.fromInt
                                , toValue = toValue (\x -> Basics.max 0.7 <| Basics.min 0.9 <| Basics.abs <| Basics.sin x)
                                , toColor = W.Chart.Colors.colorByIndex W.Chart.Colors.warm
                                }
                        , z =
                            W.Chart.axisList
                                [ W.Chart.axisLabel "Z Axis"
                                , W.Chart.distribution
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
                              -- W.Chart.Line.fromY [ W.Chart.Line.showLabels ]
                              -- W.Chart.Bar.fromY [ W.Chart.Bar.showLabels ]
                             W.Chart.Bar.fromYZ [ W.Chart.Bar.showLabels ]
                            , W.Chart.Tooltip.fromYZ
                                [ W.Chart.Tooltip.yAxisLabel [ H.text "YYY" ]
                                , W.Chart.Tooltip.axisValue
                                    (\_ xs ->
                                        xs
                                        |> List.map .value
                                        |> List.sum
                                        |> formatDecimals 2
                                        |> H.text
                                        |> List.singleton
                                    )
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


formatDecimals : Int -> Float -> String
formatDecimals numDecimals v =
    case String.split "." (String.fromFloat v) of
        [int, decimals] ->
            toDecimals numDecimals int decimals

        [int] ->
            toDecimals numDecimals int ""

        _ ->
            toDecimals numDecimals "0" ""

toDecimals : Int -> String -> String -> String
toDecimals numDecimals int decimals =
    int ++ "." ++ (decimals |> String.left numDecimals |> String.padLeft numDecimals '0')




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

-- Dark Theme

darkTheme : W.Theme.Theme
darkTheme =
    W.Theme.darkTheme
        |> W.Theme.withHeadingFont "Inter, system-ui, sans-serif"
        |> W.Theme.withTextFont "Inter, system-ui, sans-serif"
        |> W.Theme.withBaseColor
            { bg = Color.rgb255 36 41 48
            , bgSubtle = Color.rgb255 30 35 41
            , tint = Color.rgb255 45 50 58
            , tintSubtle = Color.rgb255 41 46 54
            , tintStrong = Color.rgb255 48 54 62
            , accent = Color.rgb255 85 93 105
            , accentSubtle = Color.rgb255 70 76 87
            , accentStrong = Color.rgb255 101 110 124
            , solid = Color.rgb255 78 86 100
            , solidSubtle = Color.rgb255 60 67 81
            , solidStrong = Color.rgb255 84 92 106
            , solidText = Color.rgb255 255 255 255
            , text = Color.rgb255 214 222 237
            , textSubtle = Color.rgb255 140 146 158
            , shadow = Color.rgb255 6 11 22
            }
        |> W.Theme.withPrimaryColor
            { bg = Color.rgb255 36 41 48
            , bgSubtle = Color.rgb255 30 35 41
            , tint = Color.rgb255 45 50 56
            , tintSubtle = Color.rgb255 42 46 53
            , tintStrong = Color.rgb255 49 54 60
            , accent = Color.rgb255 90 93 98
            , accentSubtle = Color.rgb255 72 76 81
            , accentStrong = Color.rgb255 107 109 114
            , solid = Color.rgb255 223 225 230
            , solidSubtle = Color.rgb255 179 181 186
            , solidStrong = Color.rgb255 238 240 245
            , solidText = Color.rgb255 0 0 0
            , text = Color.rgb255 220 222 226
            , textSubtle = Color.rgb255 143 146 151
            , shadow = Color.rgb255 10 11 14
            }
        |> W.Theme.withSuccessColor
            { bg = Color.rgb255 36 41 48
            , bgSubtle = Color.rgb255 30 35 41
            , tint = Color.rgb255 40 53 45
            , tintSubtle = Color.rgb255 38 48 47
            , tintStrong = Color.rgb255 41 58 44
            , accent = Color.rgb255 26 111 13
            , accentSubtle = Color.rgb255 33 89 26
            , accentStrong = Color.rgb255 20 134 0
            , solid = Color.rgb255 119 223 59
            , solidSubtle = Color.rgb255 79 183 0
            , solidStrong = Color.rgb255 132 237 76
            , solidText = Color.rgb255 0 0 0
            , text = Color.rgb255 157 251 105
            , textSubtle = Color.rgb255 105 164 78
            , shadow = Color.rgb255 0 22 0
            }
        |> W.Theme.withWarningColor
            { bg = Color.rgb255 36 41 48
            , bgSubtle = Color.rgb255 30 35 41
            , tint = Color.rgb255 50 50 44
            , tintSubtle = Color.rgb255 44 46 46
            , tintStrong = Color.rgb255 56 53 43
            , accent = Color.rgb255 123 84 13
            , accentSubtle = Color.rgb255 94 71 26
            , accentStrong = Color.rgb255 151 97 0
            , solid = Color.rgb255 255 197 61
            , solidSubtle = Color.rgb255 212 156 0
            , solidStrong = Color.rgb255 255 211 79
            , solidText = Color.rgb255 0 0 0
            , text = Color.rgb255 255 214 93
            , textSubtle = Color.rgb255 166 142 70
            , shadow = Color.rgb255 38 0 0
            }
        |> W.Theme.withDangerColor
            { bg = Color.rgb255 36 41 48
            , bgSubtle = Color.rgb255 30 35 41
            , tint = Color.rgb255 57 46 52
            , tintSubtle = Color.rgb255 48 44 50
            , tintStrong = Color.rgb255 65 47 54
            , accent = Color.rgb255 157 43 38
            , accentSubtle = Color.rgb255 118 45 45
            , accentStrong = Color.rgb255 197 41 31
            , solid = Color.rgb255 174 0 2
            , solidSubtle = Color.rgb255 149 0 0
            , solidStrong = Color.rgb255 182 0 13
            , solidText = Color.rgb255 255 255 255
            , text = Color.rgb255 255 167 151
            , textSubtle = Color.rgb255 170 113 107
            , shadow = Color.rgb255 66 0 0
            }

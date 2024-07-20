module Main exposing (main)

import Browser
import Date exposing (Date)
import Dict exposing (Dict)
import Html as H
import Html.Attributes as HA
import Theme
import Time exposing (Month(..))
import W.Chart
import W.Chart.Bar
import W.Chart.Colors
import W.Chart.Line
import W.Chart.Tooltip
import W.Styles


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


toColor : { a | y : List (W.Chart.Point y) } -> String
toColor point =
    point.y
        |> List.head
        |> Maybe.map (.render >> .color)
        |> Maybe.withDefault "black"



-- 1. What is our data?


date : Int -> Month -> Int -> Date
date =
    Date.fromCalendarDate


dates : List Date
dates =
    List.range 1 10
        |> List.map (\i -> Date.fromCalendarDate 2023 Jan i)


impressionsList : List ( Date, Float )
impressionsList =
    [ ( date 2023 Jan 1, 1000 )
    , ( date 2023 Jan 2, 2000 )
    , ( date 2023 Jan 3, 2500 )
    , ( date 2023 Jan 4, 3000 )
    , ( date 2023 Jan 5, 1800 )
    , ( date 2023 Jan 6, 1600 )
    , ( date 2023 Jan 7, 1200 )
    ]


purchasesList : List ( Date, Float )
purchasesList =
    [ ( date 2023 Jan 1, 10 )

    -- , ( date 2023 Jan 2, 7 )
    , ( date 2023 Jan 3, 13 )
    , ( date 2023 Jan 4, 20 )
    , ( date 2023 Jan 5, 24 )
    , ( date 2023 Jan 6, 18 )
    , ( date 2023 Jan 7, -10 )
    , ( date 2023 Jan 8, 12 )
    ]



-- Some Helpers (for now)


purchasesDict : Dict Int Float
purchasesDict =
    purchasesList
        |> List.map (Tuple.mapFirst Date.toRataDie)
        |> Dict.fromList


purchasesByDay : Date -> Maybe Float
purchasesByDay day =
    Dict.get (Date.toRataDie day) purchasesDict



-- 2. Exploring visualizations


main : Program () Model Msg
main =
    Browser.sandbox
        { init = { onClickDatum = Nothing, onClick = Nothing, onHover = Nothing }
        , update = update
        , view =
            \model ->
                let
                    chartConfig : W.Chart.ConfigXYZ Msg Date.Date Int TrigFunction
                    chartConfig =
                        W.Chart.fromXYZ
                            [ -- W.Chart.showLabels
                             W.Chart.onMouseLeaveChart OnMouseLeaveChart
                            ]
                            { x =
                                W.Chart.xAxis
                                    [ W.Chart.ticks 6
                                    , W.Chart.axisLabel "X Axis"
                                    ]
                                    { data = dates
                                    , toLabel = Date.format "MMM d"
                                    }
                            , y =
                                W.Chart.axisList
                                    [ W.Chart.axisLabel "Y Axis"
                                    , W.Chart.stacked
                                    ]
                                    { data = List.range 0 3
                                    , toLabel = String.fromInt
                                    , toColor = W.Chart.Colors.colorByIndex W.Chart.Colors.rainbow
                                    , toValue = \_ -> purchasesByDay
                                    }
                            , z =
                                W.Chart.axisList
                                    [ W.Chart.axisLabel "Z Axis", W.Chart.stacked ]
                                    { data = [ Cos ]
                                    , toLabel = trigFnLabel
                                    , toColor = trigFnColor
                                    , toValue = \fn x -> Just ((applyTrigFn fn (toFloat (Date.toRataDie x))))
                                    }
                            }
                            |> W.Chart.withActive (Maybe.map Tuple.first model.onClick)
                            |> W.Chart.withHover
                                [ W.Chart.onMouseEnter (\c a -> OnMouseEnter c (toColor a))
                                , W.Chart.onMouseLeave (\_ _ -> OnMouseLeave)
                                ]
                in
                viewWrapper model
                    [ chartConfig
                        |> W.Chart.view
                            [ W.Chart.Bar.fromY
                                [ W.Chart.Bar.labelsOutside
                                , W.Chart.Bar.labelsAsPercentages
                                , W.Chart.Bar.onMouseEnter (\a -> OnClickDatum (W.Chart.Colors.colorByIndex W.Chart.Colors.rainbow a))
                                ]
                            , W.Chart.Line.fromZ [ W.Chart.Line.labelsAsPercentages ]
                            , W.Chart.Tooltip.fromYZ []
                            ]
                    , chartConfig
                        |> W.Chart.view
                            [ W.Chart.Line.fromZ
                                [ W.Chart.Line.areaAlways
                                , W.Chart.Line.onMouseEnter (\_ -> OnClickDatum "pink")
                                , W.Chart.Line.onMouseLeave (\_ -> OnClickDatum "blue")
                                ]

                            -- , W.Chart.Line.fromZ []
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


sumAt : (a -> Float) -> List a -> Float
sumAt fn xs =
    List.foldl (\x acc -> fn x + acc) 0 xs


pctString : Float -> String
pctString v =
    String.fromFloat (v * 100) ++ "%"



--


type TrigFunction
    = Cos
    | Sin


applyTrigFn : TrigFunction -> Float -> Float
applyTrigFn v =
    case v of
        Cos ->
            \x -> cos (x * 0.5)

        Sin ->
            \x -> sin (x * 0.5)


trigFnLabel : TrigFunction -> String
trigFnLabel v =
    case v of
        Cos ->
            "Cos"

        Sin ->
            "Sin"


trigFnColor : TrigFunction -> String
trigFnColor v =
    let
        index : Int
        index =
            case v of
                Cos ->
                    0

                Sin ->
                    1
    in
    W.Chart.Colors.colorByIndex W.Chart.Colors.purple index


type alias Data =
    { label : String
    , color : String
    , toValue : Float -> Float
    }


zDataset : List Data
zDataset =
    [ ( "(x-1)^2", \x -> 2 ^ (x - 1) )
    , ( "x^2", \x -> 2 ^ x )
    ]
        |> List.indexedMap
            (\index ( label, fn ) ->
                { label = label
                , color = W.Chart.Colors.colorByIndex W.Chart.Colors.mix (index + 3)
                , toValue = fn
                }
            )


viewWrapper : Model -> List (H.Html msg) -> H.Html msg
viewWrapper model children =
    H.div
        [ HA.style "background" Theme.baseBackground
        , HA.style "min-height" "100vh"
        , HA.style "padding" "20px"
        , HA.style "display" "flex"
        , HA.style "align-items" "center"
        , HA.style "justify-content" "center"
        ]
        [ W.Styles.globalStyles
        , Theme.globalProvider Theme.darkTheme
        , W.Chart.globalStyles
        , globalStyles
        , viewColor ( 20, 20 ) (Maybe.map Tuple.second model.onClick)
        , viewColor ( 80, 20 ) (Maybe.map Tuple.second model.onHover)
        , viewColor ( 140, 20 ) model.onClickDatum
        , children
            |> List.map
                (\c ->
                    H.div
                        [ HA.style "background" Theme.baseBackground
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

.ew-charts--tooltip {
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

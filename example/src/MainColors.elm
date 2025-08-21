module Main exposing (main)

import Html as H
import Html.Attributes as HA
import W.Chart
import W.Chart.Colors
import W.Container
import W.Heading
import W.Styles
import W.Text


main : H.Html msg
main =
    let
        data : List String
        data =
            [ "Increase ROI"
            , "Growth"
            , "Budget Optimization"
            ]

        dataWithColors : List ( String, String )
        dataWithColors =
            data
                |> W.Chart.Colors.mapWithColors
                    W.Chart.Colors.mixC
                    (\color value -> (value, color))
    in
    H.div
        []
        [ W.Styles.globalStyles
        , W.Styles.baseTheme
        , W.Chart.globalStyles
        , H.div
            [ HA.style "padding" "16px"
            , HA.style "background" "#222"
            , HA.style "display" "flex"
            , HA.style "flex-direction" "column"
            ]
            (dataWithColors
                |> List.map
                    (\( value, color ) ->
                        H.div
                            [ HA.style "width" "120px"
                            , HA.style "min-height" "20px"
                            , HA.style "background" color
                            ]
                            [
                                H.text value
                            ]
                    )
            )
            -- (List.range 0 8
            --     |> W.Chart.Colors.mapWithColorsAndOffset 0
            --         W.Chart.Colors.mixC
            --         (\color _ ->
            --             H.div
            --                 [ HA.style "width" "120px"
            --                 , HA.style "height" "20px"
            --                 , HA.style "background" color
            --                 ]
            --                 []
            --         )
            -- )
        ]



-- , H.div
--     [ HA.style "gap" "16px"
--     , HA.style "padding" "16px"
--     , HA.style "background" "#eee"
--     , HA.style "display" "grid"
--     , HA.style "grid-template-columns" "repeat(2, minmax(0, 1fr))"
--     ]
--     [ viewPaletteList
--         [ ( "Mix A", W.Chart.Colors.mixA )
--         , ( "Mix B", W.Chart.Colors.mixB )
--         , ( "Mix C", W.Chart.Colors.mixC )
--         , ( "Mix D", W.Chart.Colors.mixD )
--         , ( "Mix E", W.Chart.Colors.mixE )
--         , ( "Mix Cool", W.Chart.Colors.mixCool )
--         , ( "Mix Warm", W.Chart.Colors.mixWarm )
--         ]
--     , Theme.provider Theme.darkTheme
--         []
--         [ viewPaletteList
--             [ ( "Mix A", W.Chart.Colors.mixA )
--             , ( "Mix B", W.Chart.Colors.mixB )
--             , ( "Mix C", W.Chart.Colors.mixC )
--             , ( "Mix D", W.Chart.Colors.mixD )
--             , ( "Mix E", W.Chart.Colors.mixE )
--             , ( "Mix Cool", W.Chart.Colors.mixCool )
--             , ( "Mix Warm", W.Chart.Colors.mixWarm )
--             ]
--         ]
--     , viewPaletteList
--         [ ( "Cool", W.Chart.Colors.cool )
--         , ( "Warm", W.Chart.Colors.warm )
--         , ( "Rainbow", W.Chart.Colors.rainbow )
--         , ( "Mix", W.Chart.Colors.mix )
--         , ( "Mix Alt", W.Chart.Colors.mixB )
--         ]
--     , viewPaletteList
--         [ ( "Rose", W.Chart.Colors.rose )
--         , ( "Red", W.Chart.Colors.red )
--         , ( "Orange", W.Chart.Colors.orange )
--         , ( "Amber", W.Chart.Colors.amber )
--         , ( "Yellow", W.Chart.Colors.yellow )
--         , ( "Lime", W.Chart.Colors.lime )
--         , ( "Green", W.Chart.Colors.green )
--         , ( "Emerald", W.Chart.Colors.emerald )
--         , ( "Teal", W.Chart.Colors.teal )
--         , ( "Cyan", W.Chart.Colors.cyan )
--         , ( "Sky", W.Chart.Colors.sky )
--         , ( "Blue", W.Chart.Colors.blue )
--         , ( "Indigo", W.Chart.Colors.indigo )
--         , ( "Violet", W.Chart.Colors.violet )
--         , ( "Purple", W.Chart.Colors.purple )
--         , ( "Pink", W.Chart.Colors.pink )
--         , ( "Gray", W.Chart.Colors.gray )
--         ]
--     ]
-- ]


viewPaletteList : List ( String, W.Chart.Colors.Palette ) -> H.Html msg
viewPaletteList paletteList =
    paletteList
        |> List.map viewPalette
        |> W.Container.view [ W.Container.gap_2 ]


viewPalette : ( String, W.Chart.Colors.Palette ) -> H.Html msg
viewPalette ( name, palette ) =
    let
        colorsCount : Int
        colorsCount =
            W.Chart.Colors.toColorWithShades palette 1
                |> List.length
    in
    W.Container.view
        [ W.Container.card
        , W.Container.pad_2
        , W.Container.gap_1
        ]
        [ W.Container.view
            [ W.Container.horizontal
            , W.Container.alignTop
            , W.Container.gap_1
            ]
            [ W.Heading.view
                [ W.Heading.extraSmall
                , W.Heading.htmlAttrs [ HA.style "line-height" "1em" ]
                ]
                [ H.text name ]
            , W.Text.view
                [ W.Text.extraSmall
                , W.Text.aux
                , W.Text.htmlAttrs [ HA.style "line-height" "1em" ]
                ]
                [ H.text (String.fromInt colorsCount)
                , H.text " colors (max "
                , H.text (String.fromInt (colorsCount * 4))
                , H.text ")"
                ]
            ]
        , H.div
            [ HA.style "display" "grid"
            , HA.style "grid-template-columns" "repeat(4, minmax(0, 1fr))"
            , HA.style "gap" "8px"
            ]
            [ W.Container.view [ W.Container.gap_1 ] (List.map viewColor (W.Chart.Colors.toColorWithShades palette 1))
            , W.Container.view [ W.Container.gap_1 ] (List.map viewColor (W.Chart.Colors.toColorWithShades palette 2))
            , W.Container.view [ W.Container.gap_1 ] (List.map viewColor (W.Chart.Colors.toColorWithShades palette 3))
            , W.Container.view [ W.Container.gap_1 ] (List.map viewColor (W.Chart.Colors.toColorWithShades palette 4))
            ]
        ]


viewColor : String -> H.Html msg
viewColor color =
    H.div
        [ HA.style "background" color
        , HA.style "border-radius" "4px"
        , HA.style "height" "12px"
        ]
        []

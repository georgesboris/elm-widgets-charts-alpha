module W.Chart.Colors exposing
    ( mapWithColors, mapWithColorsAndOffset, colorByIndex, Palette
    , rainbow, cool, warm
    , mixA, mixB, mixC, mixD, mixE, mixCool, mixWarm
    , amber, blue, cyan, sky, emerald, gray, lime, green, indigo, orange, pink, purple, red, rose, teal, violet, yellow
    , toColors, toColorWithShades
    , mix
    )

{-| Accessible colors based on <https://www.s-ings.com/scratchpad/oklch-smooth/> .


# Using palettes

@docs mapWithColors, mapWithColorsAndOffset, colorByIndex, Palette


# Palettes


## Basic

@docs rainbow, cool, warm


## Mixed

@docs mixA, mixB, mixC, mixD, mixE, mixCool, mixWarm


## Single Colors

@docs amber, blue, cyan, sky, emerald, gray, lime, green, indigo, orange, pink, purple, red, rose, teal, violet, yellow


# Palette Utilities

@docs toColors, toColorWithShades

Deprecated:

@docs mix

-}

import Array



-- Palette


{-| -}
type Palette
    = Palette Int Color (Array.Array Color)


singleton : Color -> Palette
singleton c =
    Palette 1 c (Array.fromList [ c ])


fromList : Color -> List Color -> Palette
fromList c cs =
    Palette (List.length cs + 1) c (Array.fromList (c :: cs))



--


{-| -}
toColorWithShades : Palette -> Int -> List String
toColorWithShades ((Palette length _ _) as palette) count =
    List.range 0 ((length * count) - 1)
        |> mapWithColorsAndOffset 0 palette (\c _ -> c)


{-| -}
mapWithColors : Palette -> (String -> a -> b) -> List a -> List b
mapWithColors =
    mapWithColorsAndOffset 0


{-| -}
mapWithColorsAndOffset : Int -> Palette -> (String -> a -> b) -> List a -> List b
mapWithColorsAndOffset offset (Palette paletteLength baseColor colors) fn xs =
    let
        itemsLength : Int
        itemsLength =
            List.length xs
    in
    xs
        |> List.indexedMap
            (\index x ->
                let
                    shadeIndex : Int
                    shadeIndex =
                        modBy shadesLength index

                    indexMod : Int
                    indexMod =
                        if itemsLength < paletteLength then
                            index

                        else
                            index // (itemsLength // paletteLength)

                    colorIndex : Int
                    colorIndex =
                        indexMod
                            + offset
                            |> modBy paletteLength

                    color : Color
                    color =
                        colors
                            |> Array.get colorIndex
                            |> Maybe.withDefault baseColor

                    shade : String
                    shade =
                        shades
                            |> Array.get shadeIndex
                            |> Maybe.withDefault .l60
                            |> (|>) color
                in
                fn shade x
            )


shades : Array.Array (Color -> String)
shades =
    Array.fromList [ .l60, .l40, .l70, .l50, .l30 ]


shadesLength : Int
shadesLength =
    Array.length shades



-- Colors


{-| -}
toColors : Palette -> List String
toColors (Palette _ _ colors) =
    List.map baseShade (Array.toList colors)


{-| -}
colorByIndex : Palette -> Int -> String
colorByIndex (Palette paletteLength baseColor colors) index =
    let
        shadeLength : Int
        shadeLength =
            4

        shadeArray : Array.Array (Color -> String)
        shadeArray =
            shades4

        shadeIndex : Int
        shadeIndex =
            modBy shadeLength index

        colorIndex : Int
        colorIndex =
            modBy paletteLength (index // shadeLength)

        color : Color
        color =
            colors
                |> Array.get (modBy paletteLength colorIndex)
                |> Maybe.withDefault baseColor
    in
    case Array.get shadeIndex shadeArray of
        Just colorToShade ->
            colorToShade color

        Nothing ->
            baseShade baseColor



-- Palettes


{-| -}
rose : Palette
rose =
    singleton colorRose


{-| -}
red : Palette
red =
    singleton colorRed


{-| -}
orange : Palette
orange =
    singleton colorOrange


{-| -}
amber : Palette
amber =
    singleton colorAmber


{-| -}
yellow : Palette
yellow =
    singleton colorYellow


{-| -}
lime : Palette
lime =
    singleton colorLime


{-| -}
green : Palette
green =
    singleton colorGreen


{-| -}
emerald : Palette
emerald =
    singleton colorEmerald


{-| -}
teal : Palette
teal =
    singleton colorTeal


{-| -}
cyan : Palette
cyan =
    singleton colorCyan


{-| -}
sky : Palette
sky =
    singleton colorSky


{-| -}
blue : Palette
blue =
    singleton colorBlue


{-| -}
indigo : Palette
indigo =
    singleton colorIndigo


{-| -}
violet : Palette
violet =
    singleton colorViolet


{-| -}
purple : Palette
purple =
    singleton colorPurple


{-| -}
pink : Palette
pink =
    singleton colorPink


{-| -}
gray : Palette
gray =
    singleton colorGray


{-| -}
warm : Palette
warm =
    fromList colorYellow
        [ colorAmber
        , colorOrange
        , colorRed
        , colorRose
        , colorPink
        , colorPurple
        , colorViolet
        ]


{-| -}
mixCool : Palette
mixCool =
    fromColorSets2 blues greens


{-| -}
mixWarm : Palette
mixWarm =
    fromColorSets3 reds yellows purples


{-| -}
cool : Palette
cool =
    fromList colorIndigo
        [ colorBlue
        , colorCyan
        , colorTeal
        , colorEmerald
        , colorGreen
        , colorLime
        ]


{-| -}
rainbow : Palette
rainbow =
    fromList
        colorRose
        [ colorRed
        , colorOrange
        , colorAmber
        , colorYellow
        , colorLime
        , colorGreen
        , colorEmerald
        , colorTeal
        , colorCyan
        , colorSky
        , colorBlue
        , colorIndigo
        , colorViolet
        , colorPurple
        , colorPink
        ]


type alias ColorSet =
    ( Color, List Color )


reds : ColorSet
reds =
    ( colorPink
    , [ colorRose
      , colorRed
      ]
    )


yellows : ColorSet
yellows =
    ( colorOrange
    , [ colorAmber
      , colorYellow
      ]
    )


blues : ColorSet
blues =
    ( colorSky
    , [ colorBlue
      , colorCyan
      , colorTeal
      , colorIndigo
      ]
    )


greens : ColorSet
greens =
    ( colorEmerald
    , [ colorGreen
      , colorLime
      ]
    )


purples : ColorSet
purples =
    ( colorViolet
    , [ colorPurple
      ]
    )


fromColorSets2 : ColorSet -> ColorSet -> Palette
fromColorSets2 ( xh, xs ) ( yh, ys ) =
    List.map2 (\x y -> [ y, x ]) xs (yh :: ys)
        |> List.concat
        |> fromList xh


fromColorSets3 : ColorSet -> ColorSet -> ColorSet -> Palette
fromColorSets3 ( xh, xs ) ( yh, ys ) ( zh, zs ) =
    List.map3 (\x y z -> [ y, z, x ]) xs (yh :: ys) (zh :: zs)
        |> List.concat
        |> fromList xh


fromColorSets5 : ColorSet -> ColorSet -> ColorSet -> ColorSet -> ColorSet -> Palette
fromColorSets5 ( xh, xs ) ( yh, ys ) ( zh, zs ) ( kh, ks ) ( wh, ws ) =
    List.map5 (\x y z k w -> [ y, z, k, w, x ]) xs (yh :: ys) (zh :: zs) (kh :: ks) (wh :: ws)
        |> List.concat
        |> fromList xh


{-| -}
mixA : Palette
mixA =
    fromColorSets5 reds yellows blues greens purples


{-| -}
mixB : Palette
mixB =
    fromColorSets5 yellows greens purples reds blues


{-| -}
mixC : Palette
mixC =
    fromColorSets5 blues reds purples greens yellows


{-| -}
mixD : Palette
mixD =
    fromColorSets5 greens yellows purples reds blues


{-| -}
mixE : Palette
mixE =
    fromColorSets5 purples yellows blues reds greens


{-| -}
mix : Palette
mix =
    fromList colorIndigo
        [ colorLime
        , colorOrange
        , colorPink
        , colorTeal
        , colorAmber
        , colorCyan
        , colorGreen
        , colorRose
        , colorViolet
        , colorEmerald
        , colorPurple
        , colorRed
        , colorSky
        , colorYellow
        , colorBlue
        ]



-- Colors


colorRose : Color
colorRose =
    { l10 = "#fce9ec"
    , l20 = "#ffd2da"
    , l30 = "#ffb7c5"
    , l40 = "#ff93ab"
    , l50 = "#fc6e91"
    , l60 = "#e74b77"
    , l70 = "#be2f5c"
    , l80 = "#911541"
    , l90 = "#5a0023"
    }


colorRed : Color
colorRed =
    { l10 = "#fde9e7"
    , l20 = "#fed4d0"
    , l30 = "#ffb9b4"
    , l40 = "#ff9793"
    , l50 = "#ff6c6f"
    , l60 = "#ed4752"
    , l70 = "#c7263a"
    , l80 = "#970823"
    , l90 = "#5c0010"
    }


colorOrange : Color
colorOrange =
    { l10 = "#feeae3"
    , l20 = "#ffd4c4"
    , l30 = "#ffbba1"
    , l40 = "#ff9b76"
    , l50 = "#fc784a"
    , l60 = "#e45729"
    , l70 = "#be3c0f"
    , l80 = "#8e2500"
    , l90 = "#551400"
    }


colorAmber : Color
colorAmber =
    { l10 = "#ffecdb"
    , l20 = "#ffdcb9"
    , l30 = "#ffc587"
    , l40 = "#ffaa4a"
    , l50 = "#fa9016"
    , l60 = "#e17900"
    , l70 = "#b66000"
    , l80 = "#7d4200"
    , l90 = "#472500"
    }


colorYellow : Color
colorYellow =
    { l10 = "#feedc6"
    , l20 = "#ffe197"
    , l30 = "#ffd158"
    , l40 = "#fdc215"
    , l50 = "#edaf00"
    , l60 = "#cb9300"
    , l70 = "#9e7100"
    , l80 = "#6d4d00"
    , l90 = "#3d2a00"
    }


colorLime : Color
colorLime =
    { l10 = "#eef2d4"
    , l20 = "#e3ed9f"
    , l30 = "#d4e360"
    , l40 = "#c1d126"
    , l50 = "#aab900"
    , l60 = "#8e9b00"
    , l70 = "#6f7900"
    , l80 = "#4f5600"
    , l90 = "#2d3100"
    }


colorGreen : Color
colorGreen =
    { l10 = "#e1f5d8"
    , l20 = "#caf1b9"
    , l30 = "#a7e788"
    , l40 = "#87d65d"
    , l50 = "#6dbe3d"
    , l60 = "#57a126"
    , l70 = "#418014"
    , l80 = "#2d5c0a"
    , l90 = "#163700"
    }


colorEmerald : Color
colorEmerald =
    { l10 = "#e0f5e5"
    , l20 = "#c5eecf"
    , l30 = "#96e4ad"
    , l40 = "#5ed78a"
    , l50 = "#2ec06d"
    , l60 = "#1ca25a"
    , l70 = "#148046"
    , l80 = "#105d32"
    , l90 = "#07371c"
    }


colorTeal : Color
colorTeal =
    { l10 = "#d9f6ed"
    , l20 = "#b7eede"
    , l30 = "#78e5c9"
    , l40 = "#33d6b3"
    , l50 = "#00bc9a"
    , l60 = "#169e81"
    , l70 = "#0b7e66"
    , l80 = "#065b49"
    , l90 = "#00372b"
    }


colorCyan : Color
colorCyan =
    { l10 = "#d7f5f6"
    , l20 = "#b0eef0"
    , l30 = "#70e3e8"
    , l40 = "#33d1d8"
    , l50 = "#1db8bf"
    , l60 = "#0f9aa1"
    , l70 = "#0a7a80"
    , l80 = "#05595e"
    , l90 = "#003538"
    }


colorSky : Color
colorSky =
    { l10 = "#daf4ff"
    , l20 = "#b9eafe"
    , l30 = "#84dcfe"
    , l40 = "#4dcbf6"
    , l50 = "#09b5e3"
    , l60 = "#069ac2"
    , l70 = "#077a9b"
    , l80 = "#065871"
    , l90 = "#003344"
    }


colorBlue : Color
colorBlue =
    { l10 = "#e3f1fe"
    , l20 = "#c3e3ff"
    , l30 = "#9fd4ff"
    , l40 = "#6fc1ff"
    , l50 = "#3aadfc"
    , l60 = "#0391e6"
    , l70 = "#0073c1"
    , l80 = "#005391"
    , l90 = "#002f56"
    }


colorIndigo : Color
colorIndigo =
    { l10 = "#e8eeff"
    , l20 = "#d7e1fe"
    , l30 = "#bdcefe"
    , l40 = "#a0b8ff"
    , l50 = "#7f9dff"
    , l60 = "#6381f8"
    , l70 = "#4a62d4"
    , l80 = "#32449c"
    , l90 = "#1e2959"
    }


colorViolet : Color
colorViolet =
    { l10 = "#f1ebff"
    , l20 = "#e6dbff"
    , l30 = "#d7c5fe"
    , l40 = "#c6a9ff"
    , l50 = "#b48aff"
    , l60 = "#9c6bed"
    , l70 = "#7f50ca"
    , l80 = "#5d3996"
    , l90 = "#361c5c"
    }


colorPurple : Color
colorPurple =
    { l10 = "#fce7fc"
    , l20 = "#fbd1fb"
    , l30 = "#f9b1fb"
    , l40 = "#f190f5"
    , l50 = "#e171e7"
    , l60 = "#c458cb"
    , l70 = "#a13da7"
    , l80 = "#79237f"
    , l90 = "#4c0452"
    }


colorPink : Color
colorPink =
    { l10 = "#fee8f2"
    , l20 = "#fcd3e6"
    , l30 = "#ffb4d9"
    , l40 = "#ff8ac8"
    , l50 = "#f668b6"
    , l60 = "#dc4d9e"
    , l70 = "#b82f7f"
    , l80 = "#8c145d"
    , l90 = "#580037"
    }


colorGray : Color
colorGray =
    { l10 = "#eaf0f5"
    , l20 = "#d8e1e9"
    , l30 = "#c1cdd8"
    , l40 = "#a9b8c6"
    , l50 = "#8fa1b2"
    , l60 = "#74889b"
    , l70 = "#5b6c7c"
    , l80 = "#414e5a"
    , l90 = "#262f37"
    }



-- Color


type alias Color =
    { l10 : String
    , l20 : String
    , l30 : String
    , l40 : String
    , l50 : String
    , l60 : String
    , l70 : String
    , l80 : String
    , l90 : String
    }


baseShade : Color -> String
baseShade =
    .l50


shades1 : Array.Array (Color -> String)
shades1 =
    Array.fromList
        [ .l50 ]


shades2 : Array.Array (Color -> String)
shades2 =
    Array.fromList
        [ .l60
        , .l30
        ]


shades3 : Array.Array (Color -> String)
shades3 =
    Array.fromList
        [ .l60
        , .l40
        , .l30
        ]


shades4 : Array.Array (Color -> String)
shades4 =
    Array.fromList
        [ .l70
        , .l60
        , .l40
        , .l30
        ]

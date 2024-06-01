module W.Chart.Internal.Tick exposing (toTicks)


toTicks : Int -> List a -> List a
toTicks ticks xs =
    case xs of
        [] ->
            []

        x :: [] ->
            [ x ]

        a :: b :: [] ->
            [ a, b ]

        a :: b :: tail ->
            let
                ( middle, last ) =
                    toLastAndMiddle b tail
            in
            if ticks <= 2 then
                [ a, last ]

            else
                a :: toTicksHelper ticks middle ++ [ last ]


toTicksHelper : Int -> List a -> List a
toTicksHelper ticks xs =
    let
        interval : Int
        interval =
            List.length xs // (ticks - 1)
    in
    xs
        |> List.foldl
            (\x ( index, acc ) ->
                if index >= interval then
                    ( 0, x :: acc )

                else
                    ( index + 1, acc )
            )
            ( 0, [] )
        |> Tuple.second
        |> List.reverse



-- a | ... | b
-- a | n / 2 | b
-- a | n / 4 | b
-- a | n / 6 | b


toLastAndMiddle : a -> List a -> ( List a, a )
toLastAndMiddle x xs =
    toLastAndMiddleHelper x xs []
        |> Tuple.mapFirst List.reverse


toLastAndMiddleHelper : a -> List a -> List a -> ( List a, a )
toLastAndMiddleHelper x xs acc =
    case xs of
        [] ->
            ( acc, x )

        next :: tail ->
            toLastAndMiddleHelper next tail (x :: acc)

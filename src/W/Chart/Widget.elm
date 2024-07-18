module W.Chart.Widget exposing
    ( empty, fromX, fromY, fromZ, fromYZ
    , withBackground, withForeground
    , withLabels
    , withHover
    )

{-|

@docs empty, fromX, fromY, fromZ, fromYZ
@docs withBackground, withForeground
@docs withLabels
@docs withHover

-}

import Svg
import W.Chart
import W.Chart.Internal exposing (Widget(..))



-- Builders


{-| -}
empty : Widget msg x y z point
empty =
    Widget
        { main = Nothing
        , background = Nothing
        , foreground = Nothing
        , hover = Nothing
        , labels = Nothing
        }


{-| -}
fromX : (W.Chart.Context x y z -> Svg.Svg msg) -> W.Chart.WidgetX msg x y z a
fromX a =
    Widget
        { main = Just a
        , background = Nothing
        , foreground = Nothing
        , hover = Nothing
        , labels = Nothing
        }


{-| -}
fromY : (W.Chart.Context x y z -> Svg.Svg msg) -> W.Chart.WidgetXY msg x y z a
fromY a =
    Widget
        { main = Just a
        , background = Nothing
        , foreground = Nothing
        , hover = Nothing
        , labels = Nothing
        }


{-| -}
fromZ : (W.Chart.Context x y z -> Svg.Svg msg) -> W.Chart.WidgetXYZ msg x y z a
fromZ a =
    Widget
        { main = Just a
        , background = Nothing
        , foreground = Nothing
        , hover = Nothing
        , labels = Nothing
        }


{-| -}
fromYZ : (W.Chart.Context x y z -> Svg.Svg msg) -> W.Chart.WidgetXYZ msg x y z a
fromYZ a =
    Widget
        { main = Just a
        , background = Nothing
        , foreground = Nothing
        , hover = Nothing
        , labels = Nothing
        }



-- Options


{-| -}
withBackground :
    (W.Chart.Context x y z
     -> Svg.Svg msg
    )
    -> Widget msg x y z point
    -> Widget msg x y z point
withBackground v (Widget d) =
    Widget { d | background = Just v }


{-| -}
withForeground :
    (W.Chart.Context x y z
     -> Svg.Svg msg
    )
    -> Widget msg x y z point
    -> Widget msg x y z point
withForeground v (Widget d) =
    Widget { d | foreground = Just v }


{-| -}
withLabels :
    (W.Chart.Context x y z
     -> Svg.Svg msg
    )
    -> Widget msg x y z point
    -> Widget msg x y z point
withLabels v (Widget d) =
    Widget { d | labels = Just v }


{-| -}
withHover :
    (W.Chart.Context x y z -> W.Chart.Coordinates -> point -> Svg.Svg msg)
    -> Widget msg x y z point
    -> Widget msg x y z point
withHover v (Widget d) =
    Widget { d | hover = Just v }

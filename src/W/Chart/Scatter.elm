module W.Chart.Scatter exposing (viewY, viewZ)

{-|

@docs viewY, viewZ

-}

import W.Chart
import W.Chart.Widget



-- View


{-| -}
viewY : List attrs -> W.Chart.Widget msg x y z
viewY attrs_ =
    W.Chart.Widget.empty


{-| -}
viewZ : List attrs -> W.Chart.Widget msg x y z
viewZ attrs_ =
    W.Chart.Widget.empty

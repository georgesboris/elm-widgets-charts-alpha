module W.Chart.Internal.TickTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import W.Chart.Internal.Tick


suite : Test
suite =
    describe "W.Chart.Internal.Tick"
        [ test "toTicks"
            (\_ ->
                Expect.all
                    [ Expect.equal [ 1, 3, 5, 6 ] << W.Chart.Internal.Tick.toTicks 4
                    , Expect.equal [ 1, 4, 6 ] << W.Chart.Internal.Tick.toTicks 3
                    , Expect.equal [ 1, 6 ] << W.Chart.Internal.Tick.toTicks 2
                    ]
                    [ 1, 2, 3, 4, 5, 6 ]
            )
        ]

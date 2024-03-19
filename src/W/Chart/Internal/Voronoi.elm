module W.Chart.Internal.Voronoi exposing (view)

import Array
import BoundingBox2d
import Dict
import Pixels
import Point2d
import Polygon2d
import Svg
import TypedSvg as S
import TypedSvg.Attributes as SA
import VoronoiDiagram2d
import W.Chart.Internal


view : (W.Chart.Internal.ChartPoint x y z -> (List (Svg.Attribute msg) -> Svg.Svg msg) -> List (Svg.Svg msg)) -> W.Chart.Internal.Context x y z -> Svg.Svg msg
view fn ctx =
    let
        voronoiResult :
            Result
                (VoronoiDiagram2d.Error ( ( Float, Float ), W.Chart.Internal.ChartPoint x y z ))
                (VoronoiDiagram2d.VoronoiDiagram2d ( ( Float, Float ), W.Chart.Internal.ChartPoint x y z ) Pixels.Pixels Float)
        voronoiResult =
            ctx.points.byXY
                |> Dict.toList
                |> List.filter
                    (\( _, dp ) ->
                        let
                            yCheck : Bool
                            yCheck =
                                List.any (not << .isDefault << .render) dp.y

                            zCheck : Bool
                            zCheck =
                                List.any (not << .isDefault << .render) dp.z
                        in
                        yCheck || zCheck
                    )
                |> Array.fromList
                |> VoronoiDiagram2d.fromVerticesBy (\( ( x, y ), _ ) -> Point2d.pixels x y)

        boundingBox : BoundingBox2d.BoundingBox2d Pixels.Pixels Float
        boundingBox =
            BoundingBox2d.from
                (Point2d.pixels 0 0)
                (Point2d.pixels ctx.width ctx.height)
    in
    voronoiResult
        |> Result.map
            (\result ->
                result
                    |> VoronoiDiagram2d.polygons boundingBox
                    |> List.concatMap
                        (\( ( _, data ), polygon ) ->
                            fn data (viewPolygon polygon)
                        )
            )
        |> Result.withDefault []
        |> S.g []


viewPolygon : Polygon2d.Polygon2d Pixels.Pixels Float -> List (Svg.Attribute msg) -> Svg.Svg msg
viewPolygon polygon attrs =
    S.polygon
        ((polygon
            |> Polygon2d.vertices
            |> List.map (Point2d.toTuple Pixels.toFloat)
            |> SA.points
         )
            :: attrs
        )
        []

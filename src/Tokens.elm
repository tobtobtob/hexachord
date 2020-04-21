module Tokens exposing (Token(..), isStarter, getDirection, svgToken)

import Directions exposing (Direction)
import Hexagons.Hex exposing (Hex)
import Hexagons.Layout
import Msg exposing (Msg)
import Svg exposing (Svg)
import Svg.Attributes
import Layout exposing (layout)
import Util
import Colors

type Token = ArrowHead Direction | Starter Direction

isStarter : Token -> Bool
isStarter token =
  case token of
    Starter _ ->
      True
    _ ->
      False

getDirection : Token -> Direction
getDirection token =
  case token of
    Starter direction ->
      direction
    ArrowHead direction ->
      direction

-- the centerpoint of two points
pointAvg : (Float, Float) -> (Float, Float) -> (Float, Float)
pointAvg (x1, y1) (x2, y2) =
  ((x1 + x2) / 2, (y1 + y2) / 2)


directionArrowPoints : Hex -> Direction -> List (Float, Float)
directionArrowPoints hex direction =
  let
    centerPoint = Hexagons.Layout.hexToPoint layout hex
    hexCorners = Directions.rotatePoints (Hexagons.Layout.polygonCorners layout hex) direction
    (pointA, pointB) = Util.take2Points hexCorners
    point1 = pointAvg centerPoint pointA
    point2 = pointAvg centerPoint pointB
    point3 = pointAvg pointA pointB
  in
    [point1, point2, point3]

directionArrowSvg : Hex -> Direction -> Svg Msg
directionArrowSvg hex direction =
  let
    arrowPoints = directionArrowPoints hex direction
    arrowPointsString = Util.pointsToString arrowPoints
  in
    Svg.polygon
      [ Svg.Attributes.stroke "black"
      , Svg.Attributes.fill "gray"
      , Svg.Attributes.strokeWidth "3"
      , Svg.Attributes.points arrowPointsString
      ]
      []
      

smallHexPoints : Hex -> List (Float, Float)
smallHexPoints hex =
  let
    centerPoint = Hexagons.Layout.hexToPoint layout hex
    hexPoints = Hexagons.Layout.polygonCorners layout hex
  in
    List.map (\point -> pointAvg point centerPoint) hexPoints

starterHexSvg : Hex -> Svg Msg
starterHexSvg hex =
  Svg.polygon
    [ Svg.Attributes.stroke "black"
    , Svg.Attributes.fill Colors.activator
    , Svg.Attributes.strokeWidth "3"
    , Svg.Attributes.points (Util.pointsToString(smallHexPoints hex))
    ]
    []
    


svgToken : Hex -> Token -> Svg Msg
svgToken hex token =
  case token of
    ArrowHead direction ->
      directionArrowSvg hex direction
    Starter direction ->
      Svg.svg
      []
      [ directionArrowSvg hex direction
      , starterHexSvg hex ]


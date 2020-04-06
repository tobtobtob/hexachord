module Tokens exposing (Token(..), TokenMap, init, svgToken)

import Directions exposing (Direction(..))
import Dict
import Hexagons.Layout
import Hexagons.Hex
import String
import Svg.Attributes
import Svg
import Msg exposing (Msg)

type Token = ArrowHead Directions.Direction

type alias TokenMap = Dict.Dict (Int, Int, Int) Token

init : TokenMap
init = 
  Dict.fromList [((5, 1, -6), ArrowHead Directions.FifthDown)
                , ((2, 4, -6), ArrowHead Directions.FifthUp) ]



tokenCornerList : Hexagons.Layout.Layout -> Hexagons.Hex.Hex -> Token -> List (Float, Float)
tokenCornerList layout hex token =
  let
    hexCorners = Hexagons.Layout.polygonCorners layout hex
    doubleHexList = hexCorners ++ hexCorners
  in
    case token of
      ArrowHead FifthDown ->
        List.take 4 doubleHexList
      ArrowHead MajorThirdUp ->
        List.take 4 (List.drop 1 doubleHexList)
      ArrowHead MinorThirdDown ->
        List.take 4 (List.drop 2 doubleHexList)
      ArrowHead FifthUp ->
        List.take 4 (List.drop 3 doubleHexList)
      ArrowHead MajorThirdDown ->
        List.take 4 (List.drop 4 doubleHexList)
      ArrowHead MinorThirdUp ->
        List.take 4 (List.drop 5 doubleHexList)

       

tokenCornerListString : Hexagons.Layout.Layout -> Hexagons.Hex.Hex -> Token -> String
tokenCornerListString layout hex token =
  let
    tokenCorners = tokenCornerList layout hex token
  in
    tokenCorners |> List.map (\(a, b) -> String.fromFloat a ++ "," ++ String.fromFloat b) |> String.join ","




svgToken: Hexagons.Layout.Layout -> Hexagons.Hex.Hex -> Token -> Svg.Svg Msg
svgToken layout hex token =
  let
      (x, y) = Hexagons.Layout.hexToPoint layout hex
  in
    Svg.svg
      []
      [ Svg.polygon
          [ Svg.Attributes.stroke "black"
          , Svg.Attributes.fill "gray"
          , Svg.Attributes.strokeWidth "3"
          , Svg.Attributes.points (tokenCornerListString layout hex token)
          ]
          []
      ]
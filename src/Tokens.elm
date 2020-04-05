module Tokens exposing (Token(..), TokenMap, init, svgToken)

import Directions
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
  Dict.fromList [((5, 1, -6), ArrowHead Directions.FifthDown)]

tokenCornerList : Hexagons.Layout.Layout -> Hexagons.Hex.Hex -> Token -> String
tokenCornerList layout hex _ =
  let
    hexCorners = Hexagons.Layout.polygonCorners layout hex
    tokenCorners = List.take 4 hexCorners 
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
          , Svg.Attributes.points (tokenCornerList layout hex token)
          ]
          []
      ]
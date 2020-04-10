module TokenMap exposing (TokenMap, init, svgToken, addOrRotateToken)

import Directions exposing (Direction(..))
import Dict
import Hexagons.Layout
import Hexagons.Hex
import String
import Svg.Attributes
import Svg
import Msg exposing (Msg)
import Tokens exposing (Token(..))


type alias TokenMap = Dict.Dict (Int, Int, Int) Token

init : TokenMap
init = 
  Dict.fromList [((5, 1, -6), ArrowHead Directions.FifthDown)
                , ((2, 4, -6), ArrowHead Directions.FifthUp) ]


rotateHexPoints : List (Float, Float) -> Directions.Direction -> List (Float, Float)
rotateHexPoints hexPoints direction =
  let
    doubleHexList = hexPoints ++ hexPoints
  in
    case direction of
      FifthDown ->
        List.take 6 doubleHexList
      MajorThirdDown ->
        List.take 6 (List.drop 1 doubleHexList)
      MinorThirdUp ->
        List.take 6 (List.drop 2 doubleHexList)
      FifthUp ->
        List.take 6 (List.drop 3 doubleHexList)
      MajorThirdUp ->
        List.take 6 (List.drop 4 doubleHexList)
      MinorThirdDown ->
        List.take 6 (List.drop 5 doubleHexList)
      

tokenCornerList : Hexagons.Layout.Layout -> Hexagons.Hex.Hex -> Token -> List (Float, Float)
tokenCornerList layout hex token =
  let
    hexCorners = Hexagons.Layout.polygonCorners layout hex
  in
    case token of
      ArrowHead direction ->
        List.take 4 (rotateHexPoints hexCorners direction)
      Starter direction ->
        List.take 4 (rotateHexPoints hexCorners direction)

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



addOrRotateToken : TokenMap -> (Int, Int, Int) -> Maybe Tokens.Token -> TokenMap
addOrRotateToken tokenMap location token =
  let
    existingToken = Dict.get location tokenMap
  in
    case (existingToken, token) of
      (_, Nothing) ->
        Dict.remove location tokenMap
      (Nothing, Just newToken) ->
        Dict.update location (\_ -> Just newToken) tokenMap
      (Just (ArrowHead oldDirection), Just (ArrowHead _)) ->
        Dict.update location (\_ -> Just (ArrowHead (Directions.rotateClockWise oldDirection))) tokenMap
      (_, _) ->
        tokenMap



      
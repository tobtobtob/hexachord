
module Directions exposing (Direction(..), moveLocation, rotateClockWise, toString, fromString, rotatePoints)
type Direction = MinorThirdUp | MinorThirdDown | FifthUp | FifthDown | MajorThirdUp | MajorThirdDown

moveLocation : Direction -> (Int, Int, Int) -> (Int, Int, Int)
moveLocation direction location =
  let
    (q, r, s ) = location
  in
    case direction of
        MinorThirdDown -> (q, r + 1, s - 1)
        MinorThirdUp -> (q, r - 1 , s + 1)
        MajorThirdUp -> (q + 1 , r, s - 1)
        MajorThirdDown -> (q - 1, r, s + 1)
        FifthDown -> (q - 1, r + 1, s)
        FifthUp -> (q + 1, r - 1, s)

rotateClockWise : Direction -> Direction
rotateClockWise direction =
  case direction of
    MinorThirdUp -> FifthUp
    MinorThirdDown -> FifthDown
    MajorThirdUp -> MinorThirdDown
    MajorThirdDown -> MinorThirdUp
    FifthDown -> MajorThirdDown
    FifthUp -> MajorThirdUp

toString : Direction -> String
toString direction =
  case direction of
    MinorThirdDown -> "MinorThirdDown"
    MinorThirdUp -> "MinorThirdUp"
    MajorThirdUp -> "MajorThirdUp"
    MajorThirdDown -> "MajorThirdDown"
    FifthDown -> "FifthDown"
    FifthUp -> "FifthUp"

fromString : String -> Direction
fromString string =
  case string of
    "MinorThirdDown" -> MinorThirdDown
    "MinorThirdUp" -> MinorThirdUp
    "MajorThirdUp" -> MajorThirdUp
    "MajorThirdDown" -> MajorThirdDown
    "FifthDown" -> FifthDown
    "FifthUp" -> FifthUp
    _ -> FifthUp

rotatePoints : List (Float, Float) -> Direction -> List (Float, Float)
rotatePoints points direction =
  let
    doubleHexList = points ++ points
    len = List.length points
  in
    case direction of
      MinorThirdDown ->
        List.take len doubleHexList
      FifthDown ->
        List.take len (List.drop 1 doubleHexList)
      MajorThirdDown ->
        List.take len (List.drop 2 doubleHexList)
      MinorThirdUp ->
        List.take len (List.drop 3 doubleHexList)
      FifthUp ->
        List.take len (List.drop 4 doubleHexList)
      MajorThirdUp ->
        List.take len (List.drop 5 doubleHexList)
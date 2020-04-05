
module Directions exposing (Direction(..), moveLocation)
type Direction = MinorThirdUp | MinorThirdDown | FifthUp | FifthDown | MajorThirdUp | MajorThirdDown

moveLocation : Direction -> (Int, Int, Int) -> (Int, Int, Int)
moveLocation direction location =
  let
    (q, r, s ) = location
  in
    case direction of
        MinorThirdUp -> (q, r + 1, s - 1)
        MinorThirdDown -> (q, r - 1 , s + 1)
        MajorThirdUp -> (q + 1 , r, s - 1)
        MajorThirdDown -> (q - 1, r, s + 1)
        FifthDown -> (q - 1, r + 1, s)
        FifthUp -> (q + 1, r - 1, s)
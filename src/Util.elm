module Util exposing (pointsToString, take2Points, catMaybes)

pointsToString : List (Float, Float) -> String
pointsToString points =
  let
    pointsStringList = List.map (\(a, b) -> String.fromFloat a ++ "," ++ String.fromFloat b) points
  in 
    String.join "," pointsStringList


take2Points : List (Float, Float) -> ((Float, Float), (Float, Float))
take2Points list =
  case list of
    elem::elem2::_ ->
      (elem, elem2)
    _ ->
      ((0, 0), (0,0))


catMaybes : List (Maybe a) -> List a
catMaybes  list =
  let
    helper = \elem list2 ->
      case elem of
        Just elem2 ->
          elem2 :: list2
        Nothing ->
          list2
  in
    List.foldl helper [] list
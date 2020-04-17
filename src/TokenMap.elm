module TokenMap exposing (TokenMap, init, hasToken, addOrRotateToken)

import Directions
import Dict
import Tokens exposing (Token(..))
import Svg exposing (Svg)
import Msg exposing (Msg)


type alias TokenMap = Dict.Dict (Int, Int, Int) Token

init : TokenMap
init = 
  Dict.fromList [((5, 1, -6), ArrowHead Directions.FifthDown)
                , ((2, 4, -6), Starter Directions.FifthUp)]

hasToken : TokenMap -> (Int, Int, Int) -> Bool
hasToken tokenMap location =
  case Dict.get location tokenMap of
    Just _ -> True
    Nothing -> False


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
      (Just (Starter oldDirection), Just (Starter _)) ->
        Dict.update location (\_ -> Just (Starter (Directions.rotateClockWise oldDirection))) tokenMap
      (_, _) ->
        tokenMap

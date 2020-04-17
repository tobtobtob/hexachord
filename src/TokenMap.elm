module TokenMap exposing (TokenMap, init, hasToken, addOrRotateToken, viewTokens)

import Directions
import Dict
import Hexagons.Map
import Tokens exposing (Token(..))
import Svg exposing (Svg)
import Msg exposing (Msg)
import Util


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


viewTokens : TokenMap -> Hexagons.Map.Map -> List (Svg Msg)
viewTokens tokenMap hexMap =
  let
    tokenLocations = Dict.keys tokenMap
    maybeSvg = \(hex, token) ->
      case (hex, token) of
        ((Just hex2), (Just token2)) ->
          Just (Tokens.svgToken hex2 token2)
        _ ->
          Nothing
    maybeHexToken = List.map (\location -> (Dict.get location hexMap, Dict.get location tokenMap)) tokenLocations
  in
    Util.catMaybes (List.map maybeSvg maybeHexToken)
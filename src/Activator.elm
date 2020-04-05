module Activator exposing (Activator, init, moveActivator, isActive)

import Dict
import Hexagons.Hex exposing (Hex)
import Hexagons.Map exposing (hashHex)
import Directions
import Tokens

type alias Activator = 
  { direction : Directions.Direction
  , location : (Int, Int, Int) }

init : Activator
init =
  { direction = Directions.FifthUp, location = (1, 5, -6) }

isActive : Hex -> Activator -> Bool
isActive hex activator =
  hashHex hex == activator.location
          
moveActivator : Tokens.TokenMap -> Activator -> Activator
moveActivator tokenMap { direction, location } =
  case Dict.get location tokenMap of
    Just (Tokens.ArrowHead tokenDirection) -> 
      { direction = tokenDirection
      , location = Directions.moveLocation tokenDirection location}
    Nothing ->
      { direction = direction
      , location = Directions.moveLocation direction location}

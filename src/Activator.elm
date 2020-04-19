module Activator exposing (Activators, init, initActivators, moveActivators, playActivators, isActive, viewActivators)

import Dict
import Set
import Hexagons.Hex exposing (Hex)
import Hexagons.Map exposing (hashHex)
import Hexagons.Layout
import Directions
import Tokens
import TokenMap
import Msg exposing(Msg)
import List
import Notes
import Cmds
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Dict
import Layout
import Util
import Colors

type alias Activator = (String, (Int, Int, Int)) 
  --{ direction : String
  --, location : (Int, Int, Int) }

type alias Activators =
  Set.Set Activator

init : Activators
init =
  Set.empty

initActivators : TokenMap.TokenMap -> Activators
initActivators tokenMap =
  let
    tokenList = Dict.toList tokenMap
    starterTokenList = List.filter (\(_, token) -> Tokens.isStarter token) tokenList
    activatorList = List.map (\(location, token) -> (Directions.toString (Tokens.getDirection token), location)) starterTokenList
  in
    Set.fromList activatorList

isActive : Hex -> Activators -> Bool
isActive hex activators =
  let
    locationSet = Set.map (\(_, location) -> location) activators
  in
    Set.member (hashHex hex) locationSet
          
moveActivator : TokenMap.TokenMap -> Activator -> Activator
moveActivator tokenMap (direction, location) =
  case Dict.get location tokenMap of
    Just (Tokens.ArrowHead tokenDirection) -> 
      (Directions.toString tokenDirection
      , Directions.moveLocation tokenDirection location )
    Just (Tokens.Starter tokenDirection) ->
      (Directions.toString tokenDirection
      , Directions.moveLocation tokenDirection location)
    Nothing ->
      (direction
      , Directions.moveLocation (Directions.fromString direction) location)

moveActivators : TokenMap.TokenMap -> Activators -> Activators
moveActivators tokenMap activators =
  Set.map (moveActivator tokenMap) activators

playActivators : Activators -> TokenMap.TokenMap -> List (Cmd Msg.Msg)
playActivators activators tokenMap =
  let
    activatorList = Set.toList activators
    locationList = List.map (\(_, location) -> location) activatorList
    filteredList = List.filter (TokenMap.hasToken tokenMap) locationList
    noteList = List.map Notes.locationToTone filteredList
  in
    List.map Cmds.start noteList

svgActivator: Hex -> Svg Msg
svgActivator hex =
  Svg.svg
    []
    [ Svg.polygon
      [ Svg.Attributes.stroke Colors.hexBorder
      , Svg.Attributes.fill Colors.activator
      , Svg.Attributes.strokeWidth "3"
      , Svg.Attributes.points (Util.pointsToString (Hexagons.Layout.polygonCorners Layout.layout hex))
      , Svg.Events.onClick (Msg.PlayNote hex)
      ]
      []
    ]



viewActivators : Activators -> Hexagons.Map.Map -> List (Svg Msg) 
viewActivators activators hexMap =
  let
    activatorLocations = List.map (\(_, b) -> b) (Set.toList activators)
    hexList = List.map (\location -> Dict.get location hexMap) activatorLocations
  in
    List.map svgActivator (Util.catMaybes hexList)

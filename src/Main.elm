

module Main exposing (main)

import Browser

import Html exposing (Html)
import Html.Events
import Html.Attributes
import Svg
import Svg.Attributes
import Hexagons.Hex exposing (..)
import Hexagons.Layout exposing (..)
import Hexagons.Map exposing (..)
import Dict
import Notes
import Cmds
import Time
import Activator
import Tokens
import Directions
import TokenMap
import Msg exposing (Msg(..))
import HexMap


main = 
  Browser.element
    { init = init
    , view = view
    , subscriptions = subscriptions
    , update = update
    }

type State = Running | Paused


type alias Model = 
  { hexMap : Map
  , activators : Activator.Activators
  , tokenMap : TokenMap.TokenMap
  , state : State 
  , tokenTool : Maybe Tokens.Token }

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.state of
      Running ->
        Time.every 250 (\_ -> Tick)
      Paused ->
        Sub.none

init : () -> (Model, Cmd Msg)
init _ = ({ hexMap = rectangularPointyTopMap 7 12
  , activators =  Activator.init
  , tokenMap = TokenMap.init
  , state = Paused 
  , tokenTool = Just (Tokens.ArrowHead Directions.FifthUp) } , Cmd.none)

hexToLocation : Hexagons.Hex.Hex -> (Int, Int, Int)
hexToLocation hex = 
  (Hexagons.Hex.intQ hex
  , Hexagons.Hex.intR hex
  , Hexagons.Hex.intS hex)

update: Msg.Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    PlayNote hex ->
      ({ model | tokenMap = TokenMap.addOrRotateToken model.tokenMap (hexToLocation hex) model.tokenTool}, Cmds.start (Notes.hexToTone hex))
    Tick ->
      let
        newActivators = Activator.moveActivators model.tokenMap model.activators
      in 
        ({ model | activators = newActivators}, Cmd.batch (Activator.playActivators newActivators model.tokenMap))
    Start ->
      ({ model | state = Running, activators = Activator.initActivators model.tokenMap }, Cmd.none)
    Stop ->
      ({ model | state = Paused }, Cmd.none)
    SelectTokenTool tokenType ->
      case tokenType of
        "ArrowHead" ->
          ({model | tokenTool = Just (Tokens.ArrowHead Directions.FifthUp)}, Cmd.none)
        "Starter" ->
          ({model | tokenTool = Just (Tokens.Starter Directions.FifthUp)}, Cmd.none)
        _ ->
          ({model | tokenTool = Nothing}, Cmd.none)
        
    NoOp ->
      (model, Cmd.none)

viewMap : Model -> Html Msg
viewMap { hexMap, activators, tokenMap} =
  Svg.svg
    [ Svg.Attributes.viewBox "0 0 1800 1200"
     --Svg.Attributes.width "100%"
    --, Svg.Attributes.height "100%"
    ]
    (List.map HexMap.viewHex (Dict.values hexMap) ++
    Activator.viewActivators activators hexMap ++
    TokenMap.viewTokens tokenMap hexMap ++
    List.map HexMap.viewTone (Dict.values hexMap) ++
    List.map HexMap.viewClickableHex (Dict.values hexMap)) 

startButton : State -> Html Msg
startButton state =
  case state of
    Running ->
      Html.button [Html.Events.onClick Stop] [Svg.text "Stop"]
    Paused ->
      Html.button [Html.Events.onClick Start] [Svg.text "Start"]

viewControls : Model -> Html Msg
viewControls model =
  Html.div
    []
    [startButton model.state,
    tokenSelector model]



tokenSelector: Model -> Html Msg
tokenSelector model =
    Html.div []
        [ Html.select [Html.Events.onInput SelectTokenTool]
            [ Svg.text "Set option: "
            , Html.option [Html.Attributes.value "ArrowHead"] [Svg.text "Arrowhead"]
            , Html.option [Html.Attributes.value "Starter"] [Svg.text "Starter"]
            , Html.option [Html.Attributes.value "None"] [Svg.text "None"]
            ]
        ]

view : Model -> Html Msg
view model =
  Html.div
    []
    [ viewControls model
    , viewMap model ]




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
import Colors
import Encoder


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
  , tokenTool : Maybe Tokens.Token
  , tempo : Int }

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.state of
      Running ->
        Time.every ((60 / toFloat model.tempo) * 1000) (\_ -> Tick)
      Paused ->
        Sub.none

init : () -> (Model, Cmd Msg)
init _ = ({ hexMap = rectangularFlatTopMap 6 14
  , activators =  Activator.init
  , tokenMap = TokenMap.init
  , state = Paused 
  , tokenTool = Just (Tokens.ArrowHead Directions.FifthUp)
  , tempo = 250} , Cmd.none)

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
    UpdateTempo tempo ->
      case String.toInt tempo of
        Just intTempo ->
          ({model | tempo = intTempo  }, Cmd.none)
        Nothing ->
         (model, Cmd.none)
    NoOp ->
      (model, Cmd.none)

viewMap : Model -> Html Msg
viewMap { hexMap, activators, tokenMap} =
  Svg.svg
    [ Svg.Attributes.viewBox "0 0 1800 1200" ]
    (List.map HexMap.viewHex (Dict.values hexMap) ++
    Activator.viewActivators activators hexMap ++
    TokenMap.viewTokens tokenMap hexMap ++
    List.map HexMap.viewTone (Dict.values hexMap) ++
    List.map HexMap.viewClickableHex (Dict.values hexMap)) 

startButton : State -> Html Msg
startButton state =
  case state of
    Running ->
      Html.button [Html.Events.onClick Stop,  Html.Attributes.style "padding-left" "10px"] [Svg.text "Stop"]
    Paused ->
      Html.button [Html.Events.onClick Start,  Html.Attributes.style "padding-left" "10px"] [Svg.text "Start"]

viewControls : Model -> Html Msg
viewControls model =
  Html.div
    [ Html.Attributes.style "display" "flex"   
    , Html.Attributes.style "background-color" Colors.controlsBackground 
    ]
    [ startButton model.state
    , tokenSelector model
    , viewTempoSlider model
    , viewEncoder model ]



tokenSelector: Model -> Html Msg
tokenSelector _ =
    Html.div [ Html.Attributes.style "padding-left" "10px"
             , Html.Attributes.style "padding-right" "10px"] 
        [ 
          Html.p [ Html.Attributes.style "margin" "0px"] [Html.text "Token type"]
        , Html.select [Html.Events.onInput SelectTokenTool]
            [ Svg.text "Set option: "
            , Html.option [Html.Attributes.value "ArrowHead"] [Svg.text "Arrowhead"]
            , Html.option [Html.Attributes.value "Starter"] [Svg.text "Starter"]
            , Html.option [Html.Attributes.value "None"] [Svg.text "None"]
            ]
        ]

viewTempoSlider : Model -> Html Msg
viewTempoSlider model =
  Html.div [ Html.Attributes.style "padding-left" "10px"
             , Html.Attributes.style "padding-right" "10px"]
    [ Html.p [ Html.Attributes.style "margin" "0px"] [Html.text "Tempo"]
    ,  Html.input
      [ Html.Attributes.type_ "range"
      , Html.Attributes.min "60"
      , Html.Attributes.max "600"
      , Html.Attributes.value  <| String.fromInt model.tempo
      , Html.Events.onInput UpdateTempo 
      ] []
    ]
viewEncoder : Model -> Html Msg
viewEncoder model =
  Html.div [ Html.Attributes.style "padding-left" "10px"
             , Html.Attributes.style "padding-right" "10px"]
    [ Html.p [ Html.Attributes.style "margin" "0px"] [Html.text (Encoder.encodeTokenMap model.hexMap model.tokenMap) ]
    ]
 
view : Model -> Html Msg
view model =
  Html.div
    []
    [ viewControls model
    , viewMap model]


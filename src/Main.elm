

module Main exposing (..)

import Browser

import Html exposing (Html)
import Html.Events
import Html.Attributes
import Svg exposing (..)
import Svg.Attributes
import Svg.Events
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
import Layout
import Msg exposing (Msg(..))


main = 
  Browser.element
    { init = init
    , view = view
    , subscriptions = subscriptions
    , update = update
    }

type State = Running | Paused

type TokenTool = Maybe Tokens.Token

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
  , tokenTool = Nothing } , Cmd.none)

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

cornerListString: Hex -> String
cornerListString hex =
  hex |> polygonCorners Layout.layout |> List.map (\(a, b) -> String.fromFloat a ++ "," ++ String.fromFloat b) |> String.join ","

viewHex: Hex -> Svg Msg
viewHex hex =
  let
      (x, y) = Hexagons.Layout.hexToPoint Layout.layout hex
  in
    svg
      []
      [ polygon
          [ Svg.Attributes.stroke "blue"
          , Svg.Attributes.fill "orange"
          , Svg.Attributes.strokeWidth "3"
          , Svg.Attributes.points (cornerListString hex)
          , Svg.Events.onClick (PlayNote hex)
          ]
          []
      , text_
          [ Svg.Attributes.fill "white"
          , Svg.Attributes.x (String.fromFloat x)
          , Svg.Attributes.y (String.fromFloat y)
          , Svg.Attributes.fontSize "20" ]
          [ text (Notes.toneToString <| Notes.hexToTone <| hex),
          text (" " ++ String.fromFloat (Notes.toneToFreq (Notes.hexToTone hex))) ]
      ]

viewMap : Model -> Html Msg
viewMap { hexMap, activators, tokenMap} =
  svg
    [ Svg.Attributes.viewBox "0 0 1800 1200"
     --Svg.Attributes.width "100%"
    --, Svg.Attributes.height "100%"
    ]
    (List.map viewHex (Dict.values hexMap) ++
    Activator.viewActivators activators hexMap ++
    TokenMap.viewTokens tokenMap hexMap)

startButton : State -> Html Msg
startButton state =
  case state of
    Running ->
      Html.button [Html.Events.onClick Stop] [text "Stop"]
    Paused ->
      Html.button [Html.Events.onClick Start] [text "Start"]

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
            [ text "Set option: "
            , Html.option [Html.Attributes.value "Starter"] [text "Starter"]
            , Html.option [Html.Attributes.value "ArrowHead"] [text "Arrowhead"]
            , Html.option [Html.Attributes.value "None"] [text "None"]
            ]
        ]

view : Model -> Html Msg
view model =
  Html.div
    []
    [ viewControls model
    , viewMap model ]


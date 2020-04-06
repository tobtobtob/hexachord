

module Main exposing (..)

import Browser

import Html exposing (Html)
import Html.Events
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
import Msg exposing (Msg(..))


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
  , activator : Activator.Activator
  , tokenMap : Tokens.TokenMap
  , state: State }

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.state of
      Running ->
        Time.every 1000 (\_ -> Tick)
      Paused ->
        Sub.none

init : () -> (Model, Cmd Msg)
init _ = ({ hexMap = rectangularPointyTopMap 5 10
  , activator =  Activator.init
  , tokenMap = Tokens.init
  , state = Paused } , Cmd.none)

update: Msg.Msg -> Model -> (Model, Cmd Msg)
update msg model = 
  case msg of
    PlayNote hex ->
      (model, Cmds.start (Notes.hexToTone hex))
    Tick ->
      let
        newActivator = Activator.moveActivator model.tokenMap model.activator
      in 
        ({ model | activator = newActivator}, Cmds.start (Notes.locationToTone newActivator.location))
    Start ->
      ({ model | state = Running }, Cmd.none)
    Stop ->
      ({ model | state = Paused }, Cmd.none)
    NoOp ->
      (model, Cmd.none)

layout: Layout
layout = 
  {   orientation = orientationLayoutPointy
    , size = (75, 75)
    , origin = (100,100)
  }

cornerListString: Hex -> String
cornerListString hex =
  hex |> polygonCorners layout |> List.map (\(a, b) -> String.fromFloat a ++ "," ++ String.fromFloat b) |> String.join ","

svgPolygon: Activator.Activator -> Hex -> Svg Msg
svgPolygon activator hex =
  let
      (x, y) = Hexagons.Layout.hexToPoint layout hex
  in
    svg
      []
      [ polygon
          [ Svg.Attributes.stroke "blue"
          , Svg.Attributes.fill (if Activator.isActive hex activator then "yellow" else "orange")
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

svgTokenHex: Activator.Activator -> Hex -> Tokens.Token -> Svg Msg
svgTokenHex activator hex token =
  svg
    []
    [ polygon
        [ Svg.Attributes.stroke "blue"
        , Svg.Attributes.fill (if Activator.isActive hex activator then "yellow" else "orange")
        , Svg.Attributes.strokeWidth "3"
        , Svg.Attributes.points (cornerListString hex)
        , Svg.Events.onClick (PlayNote hex)
        ]
        []
    , Tokens.svgToken layout hex token
    ]

drawHex : Activator.Activator -> Tokens.TokenMap -> Hex -> Svg Msg
drawHex activator tokenMap hex =
  case Dict.get (hashHex hex) tokenMap of
    Just token ->
      svgTokenHex activator hex token
    Nothing ->
      svgPolygon activator hex

viewMap : Model -> Html Msg
viewMap { hexMap, activator, tokenMap} =
  svg
    [ Svg.Attributes.viewBox "0 0 1600 1600"
    , Svg.Attributes.width "1000"
    , Svg.Attributes.height "800"
    ]
    (List.map (drawHex activator tokenMap) (Dict.values hexMap))

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
    [startButton model.state]

view : Model -> Html Msg
view model =
  Html.div
    []
    [ viewControls model
    , viewMap model ]
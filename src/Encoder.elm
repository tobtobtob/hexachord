module Encoder exposing (encodeTokenMap)

import Tokens exposing (Token)
import TokenMap exposing (TokenMap)
import Hexagons.Map exposing (Map)
import Dict
import List


type EncodingToken = Empty Int | NotEmpty Token

encodingHelper : TokenMap -> (Int, Int, Int) -> List EncodingToken -> List EncodingToken
encodingHelper tokenMap location list =
  case  Dict.get location tokenMap of
    Just token ->
      NotEmpty token :: list
    Nothing ->
      case list of
        (Empty number)::rest->
          Empty (number+1)::rest
        something ->
          Empty 1::something


encodeTokenMap : Map -> TokenMap -> String
encodeTokenMap hexMap tokenMap =
  let
    hexList = Dict.keys hexMap
    encodedList = List.reverse (List.foldl (encodingHelper tokenMap) [] hexList)
  in
    String.join "" (List.map 
      (\elem ->
        case elem of
          Empty number -> String.fromInt number
          NotEmpty token -> Tokens.encodeToken token
      ) encodedList )

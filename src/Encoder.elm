module Encoder exposing (encodeTokenMap, decodeTokenMap)

import Tokens exposing (Token)
import TokenMap exposing (TokenMap)
import Hexagons.Map exposing (Map)
import Dict
import List
import Char
import Char exposing (isDigit)


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
    encodedList = List.foldl (encodingHelper tokenMap) [] hexList
  in
    String.join "" (List.map 
      (\elem ->
        case elem of
          Empty number -> String.fromInt number
          NotEmpty token -> Tokens.encodeToken token
      ) encodedList )

encodingStringHelper : Char -> List String -> List String
encodingStringHelper newChar tokenList =
  let
    tail =
      case List.tail tokenList of
        Just list -> list
        Nothing -> []
  in
    case (List.head tokenList) of
      Just head ->
        case (Char.isDigit newChar, String.all Char.isDigit head) of
          (True, True) -> [(String.fromChar newChar) ++ head] ++ tail
          (False, False) -> [(String.fromChar newChar) ++ head] ++ tail
          _ -> [String.fromChar newChar] ++ tokenList
      Nothing ->
        [String.fromChar newChar]

stringToEncodingToken : String -> EncodingToken
stringToEncodingToken str =
  if (String.all Char.isDigit str)
    then Empty (Maybe.withDefault 0 (String.toInt str))
    else
      case (Tokens.decodeToken str) of
        Just token -> NotEmpty token
        Nothing -> Empty 1


addXEmpty : EncodingToken -> List EncodingToken -> List EncodingToken
addXEmpty token tokenList =
  case token of
    Empty 1 ->
      tokenList ++ [Empty 0]
    Empty x ->
      addXEmpty (Empty (x-1)) (tokenList ++ [Empty 0])
    _ ->
      [token]


addEmpties : EncodingToken -> List EncodingToken -> List EncodingToken
addEmpties token tokenList =
  case token of
    Empty x -> (addXEmpty token []) ++ tokenList
    NotEmpty x -> token::tokenList


toToken : ((Int, Int, Int), EncodingToken) -> Maybe ((Int, Int, Int), Token)
toToken (location, encodingToken) =
  case encodingToken of
    NotEmpty token -> Just (location, token)
    Empty _ -> Nothing


decodeTokenMap : String -> Map -> TokenMap
decodeTokenMap tokenStr hexMap =
  let
    tokenList = List.map stringToEncodingToken (List.foldr encodingStringHelper [] (String.toList tokenStr))
    withEmpties = List.foldl addEmpties [] tokenList
    hexList = Dict.keys hexMap
    hexTokenList = List.map2 Tuple.pair hexList withEmpties
    tokensOnly = List.filterMap toToken hexTokenList
  in
    Dict.fromList tokensOnly



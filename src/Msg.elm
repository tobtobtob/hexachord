module Msg exposing (Msg(..))
import Hexagons.Hex
import Tokens exposing (Token(..))
type Msg = PlayNote Hexagons.Hex.Hex | NoOp | Tick | Start | Stop | SelectTokenTool String

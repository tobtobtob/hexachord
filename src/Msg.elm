module Msg exposing (Msg(..))
import Hexagons.Hex

type Msg = PlayNote Hexagons.Hex.Hex | NoOp | Tick | Start | Stop | SelectTokenTool String | UpdateTempo String

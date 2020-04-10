module Tokens exposing (Token(..))

import Directions exposing (Direction)

type Token = ArrowHead Direction | Starter Directions.Direction

module HexMap exposing (viewHex, viewTone, viewClickableHex)

import Hexagons.Hex exposing (Hex)
import Hexagons.Layout
import Svg exposing (Svg)
import Svg.Attributes
import Svg.Events
import Msg exposing (Msg)
import Layout
import Notes
import Util
import Colors


viewHex: Hex -> Svg Msg
viewHex hex =
  Svg.svg
    []
    [ Svg.polygon
        [ Svg.Attributes.stroke Colors.hexBorder
        , Svg.Attributes.fill Colors.hex
        , Svg.Attributes.strokeWidth "3"
        , Svg.Attributes.points (Util.pointsToString (Hexagons.Layout.polygonCorners Layout.layout hex))
        ]
        []
    ]

viewTone : Hex -> Svg Msg 
viewTone hex =
  let
      (x, y) = Hexagons.Layout.hexToPoint Layout.layout hex
  in
    Svg.svg
      []
      [ Svg.text_
          [ Svg.Attributes.fill "black"
          , Svg.Attributes.x (String.fromFloat (x - 15))
          , Svg.Attributes.y (String.fromFloat (y + 15))
          , Svg.Attributes.fontSize "30" ]
          [ Svg.text (Notes.noteToString <| Notes.hexToNote <| hex)]
      ]

viewClickableHex : Hex -> Svg Msg
viewClickableHex hex =
  Svg.svg
    []
    [ Svg.polygon
        [
        Svg.Attributes.points (Util.pointsToString (Hexagons.Layout.polygonCorners Layout.layout hex))
        , Svg.Attributes.pointerEvents "bounding-box"
        , Svg.Attributes.visibility "hidden"
        , Svg.Events.onClick (Msg.PlayNote hex)
        ]
        []
    ]

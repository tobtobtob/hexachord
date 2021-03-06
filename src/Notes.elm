module Notes exposing (Note, Tone, noteToFreq, toneToFreq, hexToNote, hexToTone, noteToString, toneToString, locationToTone)

import Hexagons.Hex exposing (Hex)
import Array

type alias Tone = (Note, Int)

type Note = C | Cis | D | Eb | E | F | Fis | G | Ab | A | Bb | B  

noteToFreq : Note -> Float
noteToFreq note =
  case note of
        C ->
            130.81
        Cis ->
            138.59
        D ->
            146.83
        Eb ->
            155.56
        E ->
            164.81
        F ->
            174.61
        Fis ->
            185.0
        G ->
            196.0
        Ab ->
            207.65
        A ->
            220.0
        Bb ->
            233.08
        B ->
            246.94

toneToFreq : Tone -> Float
toneToFreq (note, octave) =
  noteToFreq note * toFloat (2 ^ octave)

noteOrder : Array.Array Note
noteOrder = Array.fromList [C, Cis, D, Eb, E, F, Fis, G, Ab, A, Bb, B]


locationToTone : (Int, Int, Int) -> Tone
locationToTone (q, r, s) =
  let
    temp = q * -6 + r * -10 + s * -3
    interval = modBy 12 temp
    octave = ((temp + 6 * 12 ) // 12) - 1
  in 
    case Array.get interval noteOrder of
      Just note -> (note, octave)
      Nothing -> (D, 1)

hexToTone : Hex -> Tone
hexToTone hex =
  let
    q = Hexagons.Hex.intQ hex
    r = Hexagons.Hex.intR hex
    s = Hexagons.Hex.intS hex
  in 
    locationToTone (q, r, s)

hexToNote : Hex -> Note
hexToNote hex =
  let
    (note, _) = hexToTone hex
  in
    note

     
noteToString : Note -> String
noteToString note =
  case note of
     C -> "C"
     Cis -> "C#"
     D -> "D"
     Eb -> "Eb"
     E -> "E"
     F -> "F"
     Fis -> "F#"
     G -> "G"
     Ab -> "Ab"
     A -> "A"
     Bb -> "Bb"
     B -> "B"

toneToString : Tone -> String
toneToString (note, octave) =
  noteToString note ++ ":" ++ String.fromInt octave

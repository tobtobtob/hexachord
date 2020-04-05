port module Cmds exposing (start)

import Notes

port startRaw : Float -> Cmd msg

start : Notes.Tone -> Cmd msg 
start note =
  startRaw (Debug.log "note" (Notes.toneToFreq note))
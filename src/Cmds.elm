port module Cmds exposing (start)

import Notes

port startRaw : String -> Cmd msg

start : Notes.Tone -> Cmd msg 
start tone =
  startRaw (Notes.toneToString tone)

port module Sounds exposing (playPlaceSound)

port playSound : String -> Cmd msg

playPlaceSound : Cmd msg
playPlaceSound =
    playSound "place.mp3"
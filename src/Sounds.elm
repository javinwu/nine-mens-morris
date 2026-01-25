port module Sounds exposing (playPlaceSound)

-- Port to send sound file paths to JavaScript
port playSound : String -> Cmd msg


-- Helper function to play the placing sound
playPlaceSound : Cmd msg
playPlaceSound =
    playSound "place.mp3"
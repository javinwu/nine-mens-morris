module Types exposing (..)

-- Add your type definitions here
type Color
  = White
  | Black

type alias Board =
  List (Maybe Piece)

type alias Piece = 
  { color : Color
  , position : Int
  }
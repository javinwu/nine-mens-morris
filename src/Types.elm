module Types exposing (..)

-- Add your type definitions here
type Player
  = White
  | Black

type alias Board =
  List (Maybe Piece)

type alias Piece = 
  { player : Player
  , position : Int
  }
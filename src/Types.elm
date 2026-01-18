module Types exposing (..)

import List

-- Add your type definitions here
type Color
  = White
  | Black

type alias Position = Int

type alias Board =
  List (Maybe Piece)

type alias Piece =
  { color : Color
  , position : Int
  }

type alias GameState =
  { currentPlayer : Color
  , selectedPiece : Maybe Position
  , phase : GamePhase
  , whitePiecesPlaced : Int
  , blackPiecesPlaced : Int
  , whitePiecesLeft : Int
  , blackPiecesLeft : Int
  }

type GamePhase
  = Placement
  | Movement
  | Flying


-- Helper functions

emptyBoard : Board
emptyBoard =
  List.repeat 24 Nothing

initialGameState : GameState
initialGameState =
  { currentPlayer = White
  , selectedPiece = Nothing
  , phase = Placement
  , whitePiecesPlaced = 0
  , blackPiecesPlaced = 0
  }

playerToString : Color -> String
playerToString color =
  case color of
    White -> "White"
    Black -> "Black"
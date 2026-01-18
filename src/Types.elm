module Types exposing (..)

import List

-- Add your type definitions here
type Player
  = White
  | Black

type alias Position = Int

type alias Board =
  List (Maybe Piece)

type alias Piece =
  { player : Player
  , position : Int
  }

type alias GameState =
  { currentPlayer : Player
  , selectedPiece : Maybe Position
  , phase : GamePhase
  , whitePiecesPlaced : Int
  , blackPiecesPlaced : Int
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

playerToString : Player -> String
playerToString player =
  case player of
    White -> "White"
    Black -> "Black"
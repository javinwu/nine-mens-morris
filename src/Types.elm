module Types exposing (..)

{-| Core Type Definitions for Nine Men's Morris -}

import List


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
  , nextPhaseAfterRemove : Maybe GamePhase
  , validMovePositions : List Position
  }


type GamePhase
  = Placement
  | Movement
  | Flying
  | Removing
  | GameOver


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
  , whitePiecesLeft = 9
  , blackPiecesLeft = 9
  , nextPhaseAfterRemove = Nothing
  , validMovePositions = []
  }


playerToString : Color -> String
playerToString color =
  case color of
    White -> "White"
    Black -> "Black"

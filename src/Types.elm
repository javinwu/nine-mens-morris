module Types exposing (..)

{-| Types Module - Core Type Definitions for Nine Men's Morris

This module defines all the core data types used throughout the game:
- Player colors (White/Black)
- Board structure (24 positions)
- Game pieces
- Game state (phase, player, counts)
- Game phases (Placement/Movement/Flying)
-}


import List


-- CORE GAME TYPES

-- Player color: Each piece belongs to either White or Black player
type Color
  = White   -- White player (goes first)
  | Black   -- Black player (goes second)


-- Position on the board (0-23)
-- The board has 24 valid positions in 3 concentric squares
type alias Position = Int


-- The game board: A list of 24 positions
-- Each position can be:
--   - Nothing (empty position)
--   - Just Piece (occupied by a piece)
type alias Board =
  List (Maybe Piece)


-- A game piece
-- Tracks which player it belongs to and where it is on the board
type alias Piece =
  { color : Color      -- Which player owns this piece (White or Black)
  , position : Int     -- Where this piece is located (0-23)
  }


-- Complete game state
-- Tracks everything about the current state of the game
type alias GameState =
  { currentPlayer : Color          -- Whose turn is it?
  , selectedPiece : Maybe Position -- Which piece is selected? (for Movement phase)
  , phase : GamePhase              -- What phase is the game in?
  , whitePiecesPlaced : Int        -- How many white pieces are on the board
  , blackPiecesPlaced : Int        -- How many black pieces are on the board
  , whitePiecesLeft : Int          -- How many white pieces remain (for tracking captures)
  , blackPiecesLeft : Int          -- How many black pieces remain (for tracking captures)
  }


-- Game phases
-- The game progresses through different phases with different rules
type GamePhase
  = Placement  -- Phase 1: Players place their 9 pieces on the board
  | Movement   -- Phase 2: Players move pieces to adjacent positions
  | Flying     -- Phase 3: Players with only 3 pieces can "fly" anywhere (not yet implemented)


-- HELPER FUNCTIONS

-- Create an empty board with all 24 positions unoccupied
emptyBoard : Board
emptyBoard =
  List.repeat 24 Nothing


-- Initial game state when starting a new game
-- White goes first, starting in Placement phase with no pieces on board
initialGameState : GameState
initialGameState =
  { currentPlayer = White        -- White always starts
  , selectedPiece = Nothing      -- No piece selected initially
  , phase = Placement            -- Start in Placement phase
  , whitePiecesPlaced = 0        -- No pieces placed yet
  , blackPiecesPlaced = 0        -- No pieces placed yet
  , whitePiecesLeft = 9          -- Each player starts with 9 pieces
  , blackPiecesLeft = 9          -- Each player starts with 9 pieces
  }


-- Convert a Color to a display string
-- Used in the UI to show which player's turn it is
playerToString : Color -> String
playerToString color =
  case color of
    White -> "White"
    Black -> "Black"
module Board exposing (..)

{-| Board Module - Nine Men's Morris Board Layout and Logic

This module defines the structure of the game board:
- 24 positions arranged in 3 concentric squares
- Adjacency relationships (which positions connect to which)
- Possible mills (valid 3-in-a-row combinations)

## BOARD LAYOUT (positions 0-23)
     0 --------- 1 --------- 2
     |           |           |
     |   8 ----- 9 ----- 10  |
     |   |       |       |   |
     |   |  16 - 17 - 18 |   |
     |   |   |       |   |   |
     7 - 15- 23      19 -11 - 3
     |   |   |       |   |   |
     |   |  22 - 21 - 20 |   |
     |   |       |       |   |
     |   14 ---- 13 ---- 12  |
     |           |           |
     6 --------- 5 --------- 4
-}


import List.Extra
import Types exposing (Piece)
import Types exposing (Color)


-- BOARD DATA STRUCTURES

-- Initial empty board with 24 positions
-- Each position can hold Maybe Piece (Nothing = empty, Just piece = occupied)
pieces : List (Maybe Piece)
pieces = List.repeat 24 Nothing


-- Adjacency list for all 24 positions
-- Each position maps to a list of positions it connects to
-- Example: Position 0 connects to positions 1 and 7
-- This is used to validate moves in the Movement phase (can only move to adjacent positions)
adjacencies : List (List Int)
adjacencies
  = [[1,7], [0,2,9], [1,3], [2,4,11], [5,3], [4,6,13], [5,7], [0,6,15]       -- Outer square (0-7)
  , [9,15], [1,8,10,17], [9,11], [3,10,12,19], [11,13], [5,12,14,21], [13,15], [7,8,14,23]  -- Middle square (8-15)
  , [17,23], [9,16,18], [17,10], [11,18,20], [19,21], [13,20,22], [21,23], [15,16,22]]      -- Inner square (16-23)


-- All possible mill combinations (3 in a row)
-- A mill is formed when a player has pieces on all 3 positions in any of these combinations
-- 16 total mills: 12 along edges (3 per square) + 4 connecting lines between squares
possibleMills : List (List Int)
possibleMills
  = [[0,1,2], [2,3,4], [4,5,6], [6,7,0]           -- Outer square (4 mills)
  , [8,9,10], [10,11,12], [12,13,14], [14,15,8]   -- Middle square (4 mills)
  , [16,17,18], [18,19,20], [20,21,22], [22,23,16]  -- Inner square (4 mills)
  , [1,9,17], [3,11,19], [5,13,21], [7,15,23]]    -- Connecting lines (4 mills)

-- BOARD QUERY FUNCTIONS
-- These functions help query and check the state of the board


-- Get all adjacent positions for a given piece
-- Used in Movement phase to validate that a move is to an adjacent position
-- Example: If piece is at position 0, returns [1, 7]
getAdjacencies : Piece -> List Int
getAdjacencies piece =
  List.drop piece.position adjacencies  -- Skip to the piece's position in the adjacency list
    |> List.head                        -- Get the adjacency list for that position
    |> Maybe.withDefault []             -- Return empty list if position invalid


-- Get the color of the piece at a specific position
-- Returns Nothing if position is empty or invalid
-- Returns Just color if there's a piece at that position
-- Example: getPieceAt 5 board -> Just White (if white piece at position 5)
getPieceAt : Int -> List (Maybe Piece) -> Maybe Color
getPieceAt position board =
  board
    |> List.drop position          -- Skip to the target position
    |> List.head                   -- Get the Maybe Piece at that position
    |> Maybe.andThen identity      -- Flatten Maybe (Maybe Piece) to Maybe Piece
    |> Maybe.map .color            -- Extract color if piece exists


-- Check if a position on the board is empty
-- Returns True if position has no piece, False if occupied
-- Used to validate piece placement and movement
isPositionEmpty : Int -> List (Maybe Piece) -> Bool
isPositionEmpty position board =
  getPieceAt position board == Nothing  -- Empty if no color found


-- Check if there is ANY mill on the board
-- Returns True if any player has formed a mill (3 in a row)
-- A mill exists when all 3 positions in a possibleMills combination
-- contain pieces of the same color
--
-- NOTE: This checks for ANY mill anywhere on the board
-- For game logic, you usually want to check if a specific move formed a NEW mill
isMill : List (Maybe Piece) -> Bool
isMill gamePieces =
  let
    -- Helper function to get piece at a specific position
    pieceAt pos =
      List.drop pos gamePieces
        |> List.head
        |> Maybe.andThen identity
  in
  -- Check each possible mill combination
  List.any
    (\mill ->
      let
        -- Get the pieces at all 3 mill positions
        millPieces = List.map pieceAt mill
      in
      -- Mill is formed if all 3 positions have pieces of the same color
      case millPieces of
        [Just p1, Just p2, Just p3] ->
          p1 == p2 && p2 == p3  -- All three pieces must match
        _ ->
          False  -- Not a mill if any position is empty
    )
    possibleMills

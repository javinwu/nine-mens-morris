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

import List.Extra exposing (elemIndex)
import Types exposing (Piece)
import Types exposing (Color)

-- BOARD DATA STRUCTURES

-- Initial empty board with 24 positions
-- Each position can hold Maybe Piece (Nothing = empty, Just piece = occupied)
pieces : List (Maybe Piece)
pieces = List.repeat 24 Nothing

-- Each position maps to a list of positions it connects to
adjacencies : List (List Int)
adjacencies
  = [[1,7], [0,2,9], [1,3], [2,4,11], [5,3], [4,6,13], [5,7], [0,6,15]       -- Outer square (0-7)
  , [9,15], [1,8,10,17], [9,11], [3,10,12,19], [11,13], [5,12,14,21], [13,15], [7,8,14,23]  -- Middle square (8-15)
  , [17,23], [9,16,18], [17,10], [11,18,20], [19,21], [13,20,22], [21,23], [15,16,22]]      -- Inner square (16-23)

-- All possible mill combinations (3 in a row)
possibleMills : List (List Int)
possibleMills
  = [[0,1,2], [2,3,4], [4,5,6], [6,7,0]           -- Outer square (4 mills)
  , [8,9,10], [10,11,12], [12,13,14], [14,15,8]   -- Middle square (4 mills)
  , [16,17,18], [18,19,20], [20,21,22], [22,23,16]  -- Inner square (4 mills)
  , [1,9,17], [3,11,19], [5,13,21], [7,15,23]]    -- Connecting lines (4 mills)


-- BOARD QUERY FUNCTIONS
-- These functions help query and check the state of the board

-- Get all adjacent positions for a given piece
getAdjacencies : Piece -> List Int
getAdjacencies piece =
  List.drop piece.position adjacencies  -- Skip to the piece's position in the adjacency list
    |> List.head                        -- Get the adjacency list for that position
    |> Maybe.withDefault []             -- Return position, return empty list if position invalid


-- Get the color of the piece at a specific position
getPieceAt : Int -> List (Maybe Piece) -> Maybe Color
getPieceAt position board =
  board
    |> List.drop position          -- Skip to the target position
    |> List.head                   -- Get the Maybe Piece at that position
    |> Maybe.andThen identity      -- Flatten Maybe (Maybe Piece) to Maybe Piece
    |> Maybe.map .color            -- Extract color if piece exists

-- stupid function for stupid stupid boy
isPositionEmpty : Int -> List (Maybe Piece) -> Bool
isPositionEmpty position board =
  getPieceAt position board == Nothing  -- entire function is literally just this line


-- Check if a specific piece is part of a mill on the given board
-- Returns True if the given piece is part of a completed mill (3 in a row)
-- This checks if all 3 positions in the mill containing this piece have the same color
isMill : Piece -> List (Maybe Piece) -> Bool
isMill piece board =
  case List.Extra.elemIndex piece.position (List.concat possibleMills) of  -- find the mill containing this piece (have to use concatenated list)
    Just a -> possibleMills                                                -- Get the mill containing this piece
      |> List.drop (floor ((toFloat a) / 4))                               -- use the index to find the correct mill since used concatenated list before
      |> List.head                                                         -- get the mill positions
      |> Maybe.withDefault []                                              -- make it a maybe
      |> List.map (\pos -> getPieceAt pos board)                           -- get the color of each position
      |> List.all (\color -> color == Just piece.color)                    -- check if all colors match
    Nothing -> False                                                       -- make elm happy and satisfy the null case

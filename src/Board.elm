module Board exposing (..)

{-| Board Module - Nine Men's Morris

24 positions in 3 concentric squares with connecting lines.
Defines adjacencies and possible mills (3 in a row).

BOARD LAYOUT (positions 0-23):
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

import Types exposing (Piece, Color)


pieces : List (Maybe Piece)
pieces = List.repeat 24 Nothing


adjacencies : List (List Int)
adjacencies
  = [[1,7], [0,2,9], [1,3], [2,4,11], [5,3], [4,6,13], [5,7], [0,6,15]
  , [9,15], [1,8,10,17], [9,11], [3,10,12,19], [11,13], [5,12,14,21], [13,15], [7,8,14,23]
  , [17,23], [9,16,18], [17,19], [11,18,20], [19,21], [13,20,22], [21,23], [15,16,22]]


possibleMills : List (List Int)
possibleMills
  = [[0,1,2], [2,3,4], [4,5,6], [6,7,0]
  , [8,9,10], [10,11,12], [12,13,14], [14,15,8]
  , [16,17,18], [18,19,20], [20,21,22], [22,23,16]
  , [1,9,17], [3,11,19], [5,13,21], [7,15,23]]


getAdjacencies : Piece -> List Int
getAdjacencies piece =
  List.drop piece.position adjacencies
    |> List.head
    |> Maybe.withDefault []


getPieceAt : Int -> List (Maybe Piece) -> Maybe Color
getPieceAt position board =
  board
    |> List.drop position
    |> List.head
    |> Maybe.andThen identity
    |> Maybe.map .color


isPositionEmpty : Int -> List (Maybe Piece) -> Bool
isPositionEmpty position board =
  getPieceAt position board == Nothing


-- Check if piece forms a mill (3 in a row)
-- Checks ALL possible mills containing the piece's position
-- Note: Some positions (corners) can be part of multiple mills
isMill : Piece -> List (Maybe Piece) -> Bool
isMill piece board =
    possibleMills
        -- Find all mills that have this position
        |> List.filter (\mill -> List.member piece.position mill)
        -- Check if any of these mills are complete (all 3 positions same color)
        |> List.any (\mill ->
            mill
                -- Get the color at each position
                |> List.map (\pos -> getPieceAt pos board)
                -- Check color
                |> List.all (\color -> color == Just piece.color)
        )

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
import Array exposing (Array)


pieces : Array (Maybe Piece)
pieces = Array.repeat 24 Nothing


adjacencies : Array (List Int)
adjacencies
  = Array.fromList
      [[1,7], [0,2,9], [1,3], [2,4,11], [5,3], [4,6,13], [5,7], [0,6,15]
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
  Array.get piece.position adjacencies
    |> Maybe.withDefault []


getPieceAt : Int -> Array (Maybe Piece) -> Maybe Color
getPieceAt position board =
  Array.get position board
    |> Maybe.andThen identity
    |> Maybe.map .color


isPositionEmpty : Int -> Array (Maybe Piece) -> Bool
isPositionEmpty position board =
  getPieceAt position board == Nothing

isMill : Piece -> Array (Maybe Piece) -> Bool
isMill piece board =
    possibleMills
        |> List.filter (\mill -> List.member piece.position mill)
        |> List.any (\mill ->
            mill
                |> List.map (\pos -> getPieceAt pos board)
                |> List.all (\color -> color == Just piece.color)
        )

getMillPositions : Piece -> Array (Maybe Piece) -> List Int
getMillPositions piece board =
    possibleMills
        |> List.filter (\mill -> List.member piece.position mill)
        |> List.filter (\mill ->
            mill
                |> List.map (\pos -> getPieceAt pos board)
                |> List.all (\color -> color == Just piece.color)
        )
        |> List.head
        |> Maybe.withDefault []

getAllMillPositions : Array (Maybe Piece) -> List Int
getAllMillPositions board =
    possibleMills
        |> List.filter (\mill ->
            let
                colors = List.map (\pos -> getPieceAt pos board) mill
                firstColor = List.head colors |> Maybe.andThen identity
            in
            case firstColor of
                Just color ->
                    List.all (\c -> c == Just color) colors
                Nothing ->
                    False
        )
        |> List.concat
        |> List.foldl (\pos acc -> if List.member pos acc then acc else pos :: acc) []

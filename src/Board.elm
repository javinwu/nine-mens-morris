module Board exposing (..)

import Types exposing (Piece)
import Types exposing (Color)
import Types exposing (Color(..))

pieces : List (Maybe Piece)
pieces
  = [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]

adjacencies : List (List Int)
adjacencies
  = [[1,7], [0,2,9], [1,3], [2,4,11], [5,3], [4,6,13], [5,7], [0,6,15]
  , [9,15], [1,8,10,17], [9,11], [3,10,12,19], [11,13], [5,12,14,21], [13,15], [7,8,14,23]
  , [17,23], [9,16,18], [17,10], [11,18,20], [19,21], [13,20,22], [21,23], [15,16,22]]

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

getPieceAt : Int -> Maybe Piece
getPieceAt position =
  pieces
    |> List.drop position
    |> List.head
    |> Maybe.andThen identity
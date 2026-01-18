module Board exposing (..)

import List.Extra
import Types exposing (Piece)
import Types exposing (Color)

-- lists of board layout stuff
pieces : List (Maybe Piece)
pieces = List.repeat 24 Nothing

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

-- get board info
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

isMill : List (Maybe Piece) -> Bool
isMill gamePieces =
  let
    pieceAt pos =
      List.drop pos gamePieces
        |> List.head
        |> Maybe.andThen identity
  in
  List.any
    (\mill ->
      let
        millPieces = List.map pieceAt mill
      in
      case millPieces of
        [Just p1, Just p2, Just p3] ->
          p1 == p2 && p2 == p3
        _ ->
          False
    )
    possibleMills

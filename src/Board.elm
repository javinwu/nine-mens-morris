module Board exposing (..)

import Types exposing (Piece)
adjacencies : List (List Int)
adjacencies = [[1,7], [0,2,9], [1,3], [2,4,11], [5,3], [4,6,13], [5,7], [0,6,15]
  , [9,15], [1,8,10,17], [9,11], [3,10,12,19], [11,13], [5,12,14,21], [13,15], [7,8,14,23]
  , [17,23], [9,16,18], [17,10], [11,18,20], [19,21], [13,20,22], [21,23], [15,16,22]]

getAdjacencies : Piece -> List Int
getAdjacencies piece =
    List.drop piece.position adjacencies
        |> List.head
        |> Maybe.withDefault []
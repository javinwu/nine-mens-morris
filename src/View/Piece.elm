module View.Piece exposing (viewPiece)

import Svg exposing (Svg, circle)
import Svg.Attributes exposing (cx, cy, r, fill, stroke, strokeWidth)
import Types exposing (Piece, Player(..))
import BoardData exposing (positionToCoordinates)
import BoardData exposing (positionToCoordinates)


viewPiece : Piece -> Maybe (Svg msg)
viewPiece piece =
    positionToCoordinates piece.position
        |> Maybe.map (\(x, y) ->
            circle
                ([ cx (String.fromInt x)
                , cy (String.fromInt y)
                , r "9"
                , fill (playerColor piece.player)
                ]
                ++ pieceStroke piece.player
                )
                []
        )


{-| Returns stroke attributes based on player color
White pieces have no stroke for a clean look, black pieces have black stroke
-}
pieceStroke : Player -> List (Svg.Attribute msg)
pieceStroke player =
    case player of
        White -> []
        Black -> [ stroke "black", strokeWidth "2" ]


playerColor : Player -> String
playerColor player =
    case player of
        White -> "white"
        Black -> "black"
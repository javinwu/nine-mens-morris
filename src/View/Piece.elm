module View.Piece exposing (viewPiece)

import Svg exposing (Svg, circle)
import Svg.Attributes exposing (cx, cy, r, fill, stroke, strokeWidth)
import Svg.Events exposing (onClick)
import Types exposing (Piece, Color(..), Position)
import BoardData exposing (positionToCoordinates)


viewPiece : Maybe Position -> (Int -> msg) -> Piece -> Maybe (Svg msg)
viewPiece selectedPiece onPieceClick piece =
    let
        isSelected = selectedPiece == Just piece.position
    in
    positionToCoordinates piece.position
        |> Maybe.map (\(x, y) ->
            circle
                ([ cx (String.fromInt x)
                , cy (String.fromInt y)
                , r "9"
                , fill (playerColor piece.color)
                , onClick (onPieceClick piece.position)
                , Svg.Attributes.style "cursor: pointer;"
                ]
                ++ pieceStroke piece.color isSelected
                )
                []
        )


{-| Returns stroke attributes based on player color and selection state
Selected pieces get a bright highlight stroke, otherwise white pieces have no stroke
and black pieces have a black stroke
-}
pieceStroke : Color -> Bool -> List (Svg.Attribute msg)
pieceStroke color isSelected =
    if isSelected then
        [ stroke "#FFD700", strokeWidth "3" ]  -- gold highlight for selected piece
    else
        case color of
            White -> []
            Black -> [ stroke "black", strokeWidth "2" ]


playerColor : Color -> String
playerColor color =
    case color of
        White -> "white"
        Black -> "black"
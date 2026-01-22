module View.Piece exposing (viewPiece)

import Svg exposing (Svg, circle)
import Svg.Attributes exposing (cx, cy, r, fill, stroke, strokeWidth)
import Svg.Events exposing (onClick)
import Types exposing (Piece, Color(..), Position)
import BoardData exposing (positionToCoordinates)


viewPiece : Maybe Position -> List Position -> (Int -> msg) -> Piece -> Maybe (Svg msg)
viewPiece selectedPiece millPositions onPieceClick piece =
    let
        isSelected = selectedPiece == Just piece.position
        isInMill = List.member piece.position millPositions
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
                ++ pieceStroke piece.color isSelected isInMill
                )
                []
        )


{-| Returns stroke attributes based on player color, selection state, and mill state
Selected pieces get a gold highlight stroke
Mill pieces get a bright cyan/lime highlight stroke
Otherwise white pieces have no stroke and black pieces have a black stroke
-}
pieceStroke : Color -> Bool -> Bool -> List (Svg.Attribute msg)
pieceStroke color isSelected isInMill =
    if isSelected then
        [ stroke "#FFD700", strokeWidth "3" ]  -- gold highlight for selected piece
    else if isInMill then
        [ stroke "#00FFFF", strokeWidth "4" ]  -- bright cyan highlight for mill pieces
    else
        case color of
            White -> []
            Black -> [ stroke "black", strokeWidth "2" ]


playerColor : Color -> String
playerColor color =
    case color of
        White -> "white"
        Black -> "black"
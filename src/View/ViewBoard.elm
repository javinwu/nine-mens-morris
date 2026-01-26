module View.ViewBoard exposing (viewBoard)

import Html exposing (Html)
import Svg exposing (Svg, svg, line, circle)
import Svg.Attributes exposing (class, viewBox, x1, y1, x2, y2, cx, cy, r, stroke, strokeWidth, fill)
import Svg.Events exposing (onClick)
import Types exposing (Piece, Position)
import View.Piece exposing (viewPiece)
import BoardData exposing (boardPositions)

positionCircle : Int -> (Int, Int) -> Bool -> (Int -> msg) -> Svg msg
positionCircle index (x, y) isHighlighted onClickMsg =
    let
        styles =
            if isHighlighted then
                { fillColor = "#32CD32"
                , strokeColor = "#00FF00"
                , strokeW = "4"
                , radius = "12"
                }
            else
                { fillColor = "#8B7355"
                , strokeColor = "#5D4E37"
                , strokeW = "3"
                , radius = "10"
                }
    in
    circle
        [ cx (String.fromInt x)
        , cy (String.fromInt y)
        , r styles.radius
        , fill styles.fillColor
        , stroke styles.strokeColor
        , strokeWidth styles.strokeW
        , onClick (onClickMsg index)
        , Svg.Attributes.style "cursor: pointer; transition: all 0.2s;"
        ]
        []

viewBoard : Maybe Position -> List Position -> List Position -> List Piece -> (Int -> msg) -> (Int -> msg) -> Html msg
viewBoard selectedPiece validMovePositions millPositions pieces onPositionClick onPieceClick =
    svg
        [ class "w-full h-auto"
        , viewBox "0 0 500 500"
        ]
        ([ Svg.rect
             [ Svg.Attributes.x "0"
             , Svg.Attributes.y "0"
             , Svg.Attributes.width "500"
             , Svg.Attributes.height "500"
             , Svg.Attributes.rx "24"
             , Svg.Attributes.ry "24"
             , fill "#D2B48C"
             ]
             []
         , line [ x1 "30", y1 "30", x2 "470", y2 "30", stroke "#8B4513", strokeWidth "3" ] []
         , line [ x1 "470", y1 "30", x2 "470", y2 "470", stroke "#8B4513", strokeWidth "3" ] []
         , line [ x1 "470", y1 "470", x2 "30", y2 "470", stroke "#8B4513", strokeWidth "3" ] []
         , line [ x1 "30", y1 "470", x2 "30", y2 "30", stroke "#8B4513", strokeWidth "3" ] []
         , line [ x1 "103", y1 "103", x2 "397", y2 "103", stroke "#8B4513", strokeWidth "3" ] []
         , line [ x1 "397", y1 "103", x2 "397", y2 "397", stroke "#8B4513", strokeWidth "3" ] []
         , line [ x1 "397", y1 "397", x2 "103", y2 "397", stroke "#8B4513", strokeWidth "3" ] []
         , line [ x1 "103", y1 "397", x2 "103", y2 "103", stroke "#8B4513", strokeWidth "3" ] []
         , line [ x1 "176", y1 "176", x2 "324", y2 "176", stroke "#8B4513", strokeWidth "3" ] []
         , line [ x1 "324", y1 "176", x2 "324", y2 "324", stroke "#8B4513", strokeWidth "3" ] []
         , line [ x1 "324", y1 "324", x2 "176", y2 "324", stroke "#8B4513", strokeWidth "3" ] []
         , line [ x1 "176", y1 "324", x2 "176", y2 "176", stroke "#8B4513", strokeWidth "3" ] []
         , line [ x1 "250", y1 "30", x2 "250", y2 "176", stroke "#8B4513", strokeWidth "3" ] []
         , line [ x1 "470", y1 "250", x2 "324", y2 "250", stroke "#8B4513", strokeWidth "3" ] []
         , line [ x1 "250", y1 "470", x2 "250", y2 "324", stroke "#8B4513", strokeWidth "3" ] []
         , line [ x1 "30", y1 "250", x2 "176", y2 "250", stroke "#8B4513", strokeWidth "3" ] []
         ]
         ++ List.indexedMap (\index pos ->
             let
                 isHighlighted = List.member index validMovePositions
             in
             positionCircle index pos isHighlighted onPositionClick
         ) boardPositions
         ++ List.filterMap (viewPiece selectedPiece millPositions onPieceClick) pieces
        ) 
module View.ViewBoard exposing (viewBoard)

import Html exposing (Html)
import Svg exposing (Svg, svg, line, circle)
import Svg.Attributes exposing (class, viewBox, x1, y1, x2, y2, cx, cy, r, stroke, strokeWidth, fill)
import Svg.Events exposing (onClick)
import Types exposing (Piece, Position)
import View.Piece exposing (viewPiece)
import BoardData exposing (boardPositions)

{-| Creates a clickable circle at the given position
-}
positionCircle : Int -> (Int, Int) -> (Int -> msg) -> Svg msg
positionCircle index (x, y) onClickMsg =
    circle
        [ cx (String.fromInt x)
        , cy (String.fromInt y)
        , r "8"
        , fill "#D4A574"  -- same as board background
        , stroke "#8B4513"  -- dark brown border
        , strokeWidth "2"
        , onClick (onClickMsg index)
        , Svg.Attributes.style "cursor: pointer;"
        ]
        []


    {-Renders the Nine Men's Morris game board
    The board has 24 positions arranged in 3 concentric squares
    Players place pieces on the intersection points (circles)
    -}
viewBoard : Maybe Position -> List Piece -> (Int -> msg) -> (Int -> msg) -> Html msg
viewBoard selectedPiece pieces onPositionClick onPieceClick =
    svg
        [ class "w-full h-auto"  -- constrained responsive sizing
        , viewBox "0 0 500 500"  -- defines the coordinate system
        ]
        ([ -- Wood background
           Svg.rect
             [ x1 "0"
             , y1 "0"
             , Svg.Attributes.width "500"
             , Svg.Attributes.height "500"
             , fill "#b5814aff"  -- light wood color
             ]
             []
         -- Outer square - the largest ring with 8 positions
         , line [ x1 "50", y1 "50", x2 "350", y2 "50", stroke "#8B4513", strokeWidth "3" ] []  -- top edge
         , line [ x1 "350", y1 "50", x2 "350", y2 "350", stroke "#8B4513", strokeWidth "3" ] []  -- right edge
         , line [ x1 "350", y1 "350", x2 "50", y2 "350", stroke "#8B4513", strokeWidth "3" ] []  -- bottom edge
         , line [ x1 "50", y1 "350", x2 "50", y2 "50", stroke "#8B4513", strokeWidth "3" ] []  -- left edge

         -- Middle square - second ring with 8 pclositions
         , line [ x1 "100", y1 "100", x2 "300", y2 "100", stroke "#8B4513", strokeWidth "3" ] []  -- top
         , line [ x1 "300", y1 "100", x2 "300", y2 "300", stroke "#8B4513", strokeWidth "3" ] []  -- right
         , line [ x1 "300", y1 "300", x2 "100", y2 "300", stroke "#8B4513", strokeWidth "3" ] []  -- bottom
         , line [ x1 "100", y1 "300", x2 "100", y2 "100", stroke "#8B4513", strokeWidth "3" ] []  -- left

         -- Inner square - smallest ring with 8 positions
         , line [ x1 "150", y1 "150", x2 "250", y2 "150", stroke "#8B4513", strokeWidth "3" ] []  -- top
         , line [ x1 "250", y1 "150", x2 "250", y2 "250", stroke "#8B4513", strokeWidth "3" ] []  -- right
         , line [ x1 "250", y1 "250", x2 "150", y2 "250", stroke "#8B4513", strokeWidth "3" ] []  -- bottom
         , line [ x1 "150", y1 "250", x2 "150", y2 "150", stroke "#8B4513", strokeWidth "3" ] []  -- left

         -- Lines connecting the squares together (one on each side)
         , line [ x1 "200", y1 "50", x2 "200", y2 "150", stroke "#8B4513", strokeWidth "3" ] []  -- top connector
         , line [ x1 "350", y1 "200", x2 "250", y2 "200", stroke "#8B4513", strokeWidth "3" ] []  -- right connector
         , line [ x1 "200", y1 "350", x2 "200", y2 "250", stroke "#8B4513", strokeWidth "3" ] []  -- bottom connector
         , line [ x1 "50", y1 "200", x2 "150", y2 "200", stroke "#8B4513", strokeWidth "3" ] []  -- left connector
         ]
         -- White circles show where players can place their pieces
         -- These are at all the line intersections (24 total)
         ++ List.indexedMap (\index pos -> positionCircle index pos onPositionClick) boardPositions
         -- Render the actual game pieces on top
         ++ List.filterMap (viewPiece selectedPiece onPieceClick) pieces
        ) 
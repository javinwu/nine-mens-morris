module View.ViewBoard exposing (viewBoard)

import Html exposing (Html)
import Svg exposing (Svg, svg, line, circle)
import Svg.Attributes exposing (class, viewBox, x1, y1, x2, y2, cx, cy, r, stroke, strokeWidth, fill)
import Svg.Events exposing (onClick)
import Types exposing (Piece, Position)
import View.Piece exposing (viewPiece)
import BoardData exposing (boardPositions)

{-| Creates a clickable circle at the given position
These circles show where pieces can be placed on the board
Highlighted circles indicate valid move positions for the selected piece
-}
positionCircle : Int -> (Int, Int) -> Bool -> (Int -> msg) -> Svg msg
positionCircle index (x, y) isHighlighted onClickMsg =
    let
        styles =
            if isHighlighted then
                { fillColor = "#32CD32"  -- bright green for valid moves
                , strokeColor = "#00FF00"  -- bright green border
                , strokeW = "4"
                , radius = "12"
                }
            else
                { fillColor = "#8B7355"  -- default wood color
                , strokeColor = "#5D4E37"  -- darker brown border
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


    {-Renders the Nine Men's Morris game board
    The board has 24 positions arranged in 3 concentric squares
    Players place pieces on the intersection points (circles)
    Board is centered in the SVG with proper padding
    validMovePositions highlights positions where the selected piece can move
    millPositions highlights pieces that form a mill
    -}
viewBoard : Maybe Position -> List Position -> List Position -> List Piece -> (Int -> msg) -> (Int -> msg) -> Html msg
viewBoard selectedPiece validMovePositions millPositions pieces onPositionClick onPieceClick =
    svg
        [ class "w-full h-auto"  -- constrained responsive sizing
        , viewBox "0 0 500 500"  -- defines the coordinate system
        ]
        ([ -- Wood background for entire SVG with rounded corners
           Svg.rect
             [ Svg.Attributes.x "0"
             , Svg.Attributes.y "0"
             , Svg.Attributes.width "500"
             , Svg.Attributes.height "500"
             , Svg.Attributes.rx "24"  -- rounded corners
             , Svg.Attributes.ry "24"  -- rounded corners
             , fill "#D2B48C"  -- tan/wood color background
             ]
             []
         -- Outer square - the largest ring with 8 positions
         -- Centered: board spans from 100 to 400 (300 units), centered in 500x500
         , line [ x1 "100", y1 "100", x2 "400", y2 "100", stroke "#8B4513", strokeWidth "3" ] []  -- top edge
         , line [ x1 "400", y1 "100", x2 "400", y2 "400", stroke "#8B4513", strokeWidth "3" ] []  -- right edge
         , line [ x1 "400", y1 "400", x2 "100", y2 "400", stroke "#8B4513", strokeWidth "3" ] []  -- bottom edge
         , line [ x1 "100", y1 "400", x2 "100", y2 "100", stroke "#8B4513", strokeWidth "3" ] []  -- left edge

         -- Middle square - second ring with 8 positions
         -- Centered with 50 unit spacing from outer
         , line [ x1 "150", y1 "150", x2 "350", y2 "150", stroke "#8B4513", strokeWidth "3" ] []  -- top
         , line [ x1 "350", y1 "150", x2 "350", y2 "350", stroke "#8B4513", strokeWidth "3" ] []  -- right
         , line [ x1 "350", y1 "350", x2 "150", y2 "350", stroke "#8B4513", strokeWidth "3" ] []  -- bottom
         , line [ x1 "150", y1 "350", x2 "150", y2 "150", stroke "#8B4513", strokeWidth "3" ] []  -- left

         -- Inner square - smallest ring with 8 positions
         -- Centered with 50 unit spacing from middle
         , line [ x1 "200", y1 "200", x2 "300", y2 "200", stroke "#8B4513", strokeWidth "3" ] []  -- top
         , line [ x1 "300", y1 "200", x2 "300", y2 "300", stroke "#8B4513", strokeWidth "3" ] []  -- right
         , line [ x1 "300", y1 "300", x2 "200", y2 "300", stroke "#8B4513", strokeWidth "3" ] []  -- bottom
         , line [ x1 "200", y1 "300", x2 "200", y2 "200", stroke "#8B4513", strokeWidth "3" ] []  -- left

         -- Lines connecting the squares together (one on each side)
         -- All centered at 250 (middle of 500)
         , line [ x1 "250", y1 "100", x2 "250", y2 "200", stroke "#8B4513", strokeWidth "3" ] []  -- top connector
         , line [ x1 "400", y1 "250", x2 "300", y2 "250", stroke "#8B4513", strokeWidth "3" ] []  -- right connector
         , line [ x1 "250", y1 "400", x2 "250", y2 "300", stroke "#8B4513", strokeWidth "3" ] []  -- bottom connector
         , line [ x1 "100", y1 "250", x2 "200", y2 "250", stroke "#8B4513", strokeWidth "3" ] []  -- left connector
         ]
         -- White circles show where players can place their pieces
         -- These are at all the line intersections (24 total)
         -- Highlighted circles show valid move positions for the selected piece
         ++ List.indexedMap (\index pos ->
             let
                 isHighlighted = List.member index validMovePositions
             in
             positionCircle index pos isHighlighted onPositionClick
         ) boardPositions
         -- Render the actual game pieces on top
         ++ List.filterMap (viewPiece selectedPiece millPositions onPieceClick) pieces
        ) 
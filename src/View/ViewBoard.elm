module View.ViewBoard exposing (viewBoard)

import Html exposing (Html)
import Svg exposing (Svg, svg, line, circle)
import Svg.Attributes exposing (class, viewBox, x1, y1, x2, y2, cx, cy, r, stroke, strokeWidth, fill)


{- All 24 board positions as (x, y) coordinates
-}
boardPositions : List (Int, Int)
boardPositions =
    -- Outer square (8 positions)
    [ (50, 50), (200, 50), (350, 50)
    , (350, 200)
    , (350, 350), (200, 350), (50, 350)
    , (50, 200)
    -- Middle square (8 positions)
    , (100, 100), (200, 100), (300, 100)
    , (300, 200)
    , (300, 300), (200, 300), (100, 300)
    , (100, 200)
    -- Inner square (8 positions)
    , (150, 150), (200, 150), (250, 150)
    , (250, 200)
    , (250, 250), (200, 250), (150, 250)
    , (150, 200)
    ]

positionToCoordinates : Int -> Maybe (Int, Int)
positionToCoordinates pos = 
    List.drop pos boardPositions
        |> List.head

{-| Creates a circle at the given position
-}
positionCircle : (Int, Int) -> Svg msg
positionCircle (x, y) =
    circle
        [ cx (String.fromInt x)
        , cy (String.fromInt y)
        , r "8"
        , fill "white"
        , stroke "black"
        , strokeWidth "2"
        ]
        []


    {-Renders the Nine Men's Morris game board
    The board has 24 positions arranged in 3 concentric squares
    Players place pieces on the intersection points (circles)
    -}
viewBoard : Html msg
viewBoard =
    svg
        [ class "w-full h-auto"  -- constrained responsive sizing
        , viewBox "0 0 500 500"  -- defines the coordinate system
        ]
        ([ -- Outer square - the largest ring with 8 positions
           line [ x1 "50", y1 "50", x2 "350", y2 "50", stroke "black", strokeWidth "2" ] []  -- top edge
         , line [ x1 "350", y1 "50", x2 "350", y2 "350", stroke "black", strokeWidth "2" ] []  -- right edge
         , line [ x1 "350", y1 "350", x2 "50", y2 "350", stroke "black", strokeWidth "2" ] []  -- bottom edge
         , line [ x1 "50", y1 "350", x2 "50", y2 "50", stroke "black", strokeWidth "2" ] []  -- left edge

         -- Middle square - second ring with 8 pclositions
         , line [ x1 "100", y1 "100", x2 "300", y2 "100", stroke "black", strokeWidth "2" ] []  -- top
         , line [ x1 "300", y1 "100", x2 "300", y2 "300", stroke "black", strokeWidth "2" ] []  -- right
         , line [ x1 "300", y1 "300", x2 "100", y2 "300", stroke "black", strokeWidth "2" ] []  -- bottom
         , line [ x1 "100", y1 "300", x2 "100", y2 "100", stroke "black", strokeWidth "2" ] []  -- left

         -- Inner square - smallest ring with 8 positions
         , line [ x1 "150", y1 "150", x2 "250", y2 "150", stroke "black", strokeWidth "2" ] []  -- top
         , line [ x1 "250", y1 "150", x2 "250", y2 "250", stroke "black", strokeWidth "2" ] []  -- right
         , line [ x1 "250", y1 "250", x2 "150", y2 "250", stroke "black", strokeWidth "2" ] []  -- bottom
         , line [ x1 "150", y1 "250", x2 "150", y2 "150", stroke "black", strokeWidth "2" ] []  -- left

         -- Lines connecting the squares together (one on each side)
         , line [ x1 "200", y1 "50", x2 "200", y2 "150", stroke "black", strokeWidth "2" ] []  -- top connector
         , line [ x1 "350", y1 "200", x2 "250", y2 "200", stroke "black", strokeWidth "2" ] []  -- right connector
         , line [ x1 "200", y1 "350", x2 "200", y2 "250", stroke "black", strokeWidth "2" ] []  -- bottom connector
         , line [ x1 "50", y1 "200", x2 "150", y2 "200", stroke "black", strokeWidth "2" ] []  -- left connector
         ]
         -- White circles show where players can place their pieces
         -- These are at all the line intersections (24 total)
         ++ List.map positionCircle boardPositions
        ) 
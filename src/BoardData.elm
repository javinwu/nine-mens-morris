module BoardData exposing (positionToCoordinates, boardPositions)

import Array exposing (Array)


{-| All 24 board positions as (x, y) coordinates
Board is centered in 500x500 viewBox with 30 unit padding
Outer square: 30-470, Middle square: 103-397, Inner square: 176-324
-}
boardPositions : Array (Int, Int)
boardPositions =
    Array.fromList
        -- Outer square (8 positions) - centered in viewBox
        [ (30, 30), (250, 30), (470, 30)    -- top-left, top-center, top-right
        , (470, 250)                         -- right-center
        , (470, 470), (250, 470), (30, 470) -- bottom-right, bottom-center, bottom-left
        , (30, 250)                          -- left-center
        -- Middle square (8 positions)
        , (103, 103), (250, 103), (397, 103)  -- top-left, top-center, top-right
        , (397, 250)                           -- right-center
        , (397, 397), (250, 397), (103, 397)  -- bottom-right, bottom-center, bottom-left
        , (103, 250)                           -- left-center
        -- Inner square (8 positions)
        , (176, 176), (250, 176), (324, 176)  -- top-left, top-center, top-right
        , (324, 250)                           -- right-center
        , (324, 324), (250, 324), (176, 324)  -- bottom-right, bottom-center, bottom-left
        , (176, 250)                           -- left-center
        ]

positionToCoordinates : Int -> Maybe (Int, Int)
positionToCoordinates pos =
    Array.get pos boardPositions

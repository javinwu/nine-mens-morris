module BoardData exposing (positionToCoordinates, boardPositions)


{-| All 24 board positions as (x, y) coordinates
Board is centered in 500x500 viewBox with 100 unit padding
Outer square: 100-400, Middle square: 150-350, Inner square: 200-300
-}
boardPositions : List (Int, Int)
boardPositions =
    -- Outer square (8 positions) - centered in viewBox
    [ (100, 100), (250, 100), (400, 100)  -- top-left, top-center, top-right
    , (400, 250)                           -- right-center
    , (400, 400), (250, 400), (100, 400)  -- bottom-right, bottom-center, bottom-left
    , (100, 250)                           -- left-center
    -- Middle square (8 positions)
    , (150, 150), (250, 150), (350, 150)  -- top-left, top-center, top-right
    , (350, 250)                           -- right-center
    , (350, 350), (250, 350), (150, 350)  -- bottom-right, bottom-center, bottom-left
    , (150, 250)                           -- left-center
    -- Inner square (8 positions)
    , (200, 200), (250, 200), (300, 200)  -- top-left, top-center, top-right
    , (300, 250)                           -- right-center
    , (300, 300), (250, 300), (200, 300)  -- bottom-right, bottom-center, bottom-left
    , (200, 250)                           -- left-center
    ]

positionToCoordinates : Int -> Maybe (Int, Int)
positionToCoordinates pos =
    List.drop pos boardPositions
        |> List.head

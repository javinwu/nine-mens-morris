module BoardData exposing (positionToCoordinates, boardPositions)


{-| All 24 board positions as (x, y) coordinates
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


{-| Convert a position index to (x, y) coordinates
-}
positionToCoordinates : Int -> Maybe (Int, Int)
positionToCoordinates pos =
    List.drop pos boardPositions
        |> List.head

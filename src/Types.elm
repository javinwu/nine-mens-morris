module Types exposing (..)


{-| Represents which player owns a piece
-}
type Player
    = Player1
    | Player2


{-| Represents a position on the board (0-23)
    The board has 24 positions arranged in 3 concentric squares
-}
type alias Position =
    Int


{-| Represents a piece on the board
-}
type alias Piece =
    { player : Player
    , position : Position
    }

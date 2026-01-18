module Main exposing (main)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Svg exposing (Svg)
import Types exposing (Piece, Player(..))
import View.ViewBoard exposing (viewBoard)
import View.Piece exposing (viewPiece)


type alias Model =
    { pieces : List Piece
    , selectedPosition : Maybe Int
    }


type Msg
    = NoOp


init : Model
init =
    { pieces = []
    , selectedPosition = Nothing
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    div [ class "min-h-screen flex items-center justify-center p-4" ]
        [ div [ class "w-full max-w-md" ]
            [ viewBoard model.pieces ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
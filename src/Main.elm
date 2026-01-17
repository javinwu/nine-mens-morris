module Main exposing (main)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import View.ViewBoard exposing (viewBoard)


type alias Model =
    {}


type Msg
    = NoOp


init : Model
init =
    {}


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model


view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-gray-100 flex items-center justify-center p-4" ]
        [ div [ class "w-full max-w-md" ]
            [ viewBoard ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }

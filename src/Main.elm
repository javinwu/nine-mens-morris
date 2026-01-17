module Main exposing (main)

import Browser
import Html exposing (Html, div, text)
import Html.Attributes
import View.Board exposing (viewBoard)


-- MODEL

type alias Model =
    {}


init : Model
init =
    {}


-- UPDATE

type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    model


-- VIEW

view : Model -> Html Msg
view model =
    div [ Html.Attributes.class "min-h-screen bg-gray-100 flex items-center justify-center p-8" ]
        [ viewBoard ]


-- MAIN

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }

module Main exposing (main)

import Browser
import Html exposing (Html, div, text)


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
    div [] []


-- MAIN

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }

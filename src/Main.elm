module Main exposing (main)

import Browser
import Html exposing (Html, div, h1, text, button, p)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


-- MAIN

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


-- MODEL

type alias Model =
    { message : String
    , gameStarted : Bool
    }


init : Model
init =
    { message = "Welcome to Nine Men's Morris!"
    , gameStarted = False
    }


-- UPDATE

type Msg
    = StartGame
    | ResetGame


update : Msg -> Model -> Model
update msg model =
    case msg of
        StartGame ->
            { model 
            | message = "Game Started! Let's play."
            , gameStarted = True
            }
        
        ResetGame ->
            init


-- VIEW

view : Model -> Html Msg
view model =
    div 
        [ class "min-h-screen bg-gradient-to-br from-blue-50 to-purple-100 flex items-center justify-center p-4" ]
        [ div 
            [ class "bg-white rounded-2xl shadow-2xl p-8 max-w-2xl w-full" ]
            [ h1 
                [ class "text-4xl font-bold text-center text-gray-800 mb-6" ]
                [ text "🎮 Nine Men's Morris" ]
            , div 
                [ class "text-center mb-6" ]
                [ p 
                    [ class "text-xl text-gray-700 mb-4" ]
                    [ text model.message ]
                , p 
                    [ class "text-sm text-gray-500" ]
                    [ text "Built with Elm + Tailwind CSS on Linux 🐧" ]
                ]
            , div 
                [ class "flex gap-4 justify-center" ]
                [ if not model.gameStarted then
                    button
                        [ onClick StartGame
                        , class "bg-blue-600 hover:bg-blue-700 text-white font-semibold py-3 px-8 rounded-lg transition-colors"
                        ]
                        [ text "Start Game" ]
                  else
                    button
                        [ onClick ResetGame
                        , class "bg-gray-600 hover:bg-gray-700 text-white font-semibold py-3 px-8 rounded-lg transition-colors"
                        ]
                        [ text "Reset Game" ]
                ]
            ]
        ]

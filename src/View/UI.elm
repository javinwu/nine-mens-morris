module View.UI exposing (nextGameButton)

import Html exposing (Html, button, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

nextGameButton : msg -> Html msg
nextGameButton onClickMsg =
    button
        [ class "next-game-button"
        , onClick onClickMsg
        ]
        [ text "New Game" ]
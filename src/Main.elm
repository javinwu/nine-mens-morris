module Main exposing (main)

import Browser
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Svg exposing (Svg)
import Types exposing (Piece, Color(..))
import View.ViewBoard exposing (viewBoard)


-- MODEL
type alias Model =
    { board : Board
    , gameState : GameState
    }


init : Model
init =
    { board = emptyBoard
    , gameState = initialGameState
    }


-- MESSAGES
type Msg
    = ClickedPosition Position
    | ClickedPiece Position
    | NewGame
    | NoOp


-- UPDATE
update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedPiece pos ->
            -- Select the piece
            let
                newGameState =
                    model.gameState
                updatedGameState =
                    { newGameState | selectedPiece = Just pos }
            in
            { model | gameState = updatedGameState }

        ClickedPosition pos ->
            -- Handle piece placement during Placement phase
            if model.gameState.phase == Placement then
                let
                    currentPlayer = model.gameState.currentPlayer
                in
                -- Check if position is empty and player can place
                -- TODO: Replace True with: getPieceAt pos model.board == Nothing
                if True && canPlacePiece currentPlayer model.gameState then
                    let
                        -- Place the piece
                        newBoard = placePiece pos currentPlayer model.board

                        -- Update piece counts
                        (newWhiteCount, newBlackCount) =
                            case currentPlayer of
                                White -> (model.gameState.whitePiecesPlaced + 1, model.gameState.blackPiecesPlaced)
                                Black -> (model.gameState.whitePiecesPlaced, model.gameState.blackPiecesPlaced + 1)

                        -- Switch to next player
                        newGameState =
                            { currentPlayer = nextPlayer currentPlayer
                            , selectedPiece = Nothing
                            , phase = Placement
                            , whitePiecesPlaced = newWhiteCount
                            , blackPiecesPlaced = newBlackCount
                            }
                    in
                    { model | board = newBoard, gameState = newGameState }
                else
                    model
            else
                -- For other phases, just deselect
                let
                    newGameState = model.gameState
                    updatedGameState = { newGameState | selectedPiece = Nothing }
                in
                { model | gameState = updatedGameState }

        NewGame ->
            init

        NoOp ->
            model


-- Helper to convert Board to List of Pieces
boardToPieces : Board -> List Piece
boardToPieces board =
    board
        |> List.indexedMap (\index maybePiece ->
            Maybe.map (\piece -> piece) maybePiece
        )
        |> List.filterMap identity


-- Validation helpers
getPiecesForPlayer : Color -> Board -> List Piece
getPiecesForPlayer color board =
    boardToPieces board
        |> List.filter (\piece -> piece.color == color)


canPlacePiece : Color -> GameState -> Bool
canPlacePiece color gameState =
    let
        piecesPlaced =
            case color of
                White -> gameState.whitePiecesPlaced
                Black -> gameState.blackPiecesPlaced
    in
    piecesPlaced < 9


placePiece : Int -> Color -> Board -> Board
placePiece pos color board =
    List.indexedMap
        (\index maybePiece ->
            if index == pos then
                Just { color = color, position = pos }
            else
                maybePiece
        )
        board


nextPlayer : Color -> Color
nextPlayer color =
    case color of
        White -> Black
        Black -> White


-- VIEW
view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-gray-900 flex flex-col items-center justify-center p-8" ]
        [ -- Game header
          div [ class "text-white text-2xl mb-8" ]
            [ text ("Current Player: " ++ playerToString model.gameState.currentPlayer) ]

        , -- Board
          viewBoard (boardToPieces model.board) ClickedPosition

        , -- Controls
        -- reset button
          button
            [ class "mt-8 bg-blue-600 hover:bg-blue-700 text-white font-bold py-3 px-8 rounded-lg transition-colors"
            , onClick NewGame
            ]
            [ text "Game Reset" ]
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
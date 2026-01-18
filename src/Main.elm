module Main exposing (main)

import Browser
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Svg exposing (Svg)
import Types exposing (Piece, Color(..), Board, GameState, Position, emptyBoard, initialGameState, GamePhase(..), playerToString)
import View.ViewBoard exposing (viewBoard)
import Board exposing (getPieceAt)


-- MODEL

-- The main application state
-- Contains the game board and current game state
type alias Model =
    { board : Board
    , gameState : GameState
    }


-- Initialize a new game with an empty board
init : Model
init =
    { board = emptyBoard
    , gameState = initialGameState
    }


-- MESSAGES

-- All possible user actions in the game
type Msg
    = ClickedPosition Position  -- User clicked an empty board position
    | ClickedPiece Position     -- User clicked an existing piece
    | NewGame                   -- User wants to reset the game
    | NoOp                      -- No operation (placeholder)


-- UPDATE

-- Handle user actions and update the game state accordingly
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
                if getPieceAt pos model.board == Nothing && canPlacePiece currentPlayer model.gameState then
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
                            , whitePiecesLeft = model.gameState.whitePiecesLeft
                            , blackPiecesLeft = model.gameState.blackPiecesLeft
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


-- HELPER FUNCTIONS

-- Convert the board (list of Maybe Piece) into a list of actual pieces
-- Filters out empty positions
boardToPieces : Board -> List Piece
boardToPieces board =
    board
        |> List.indexedMap (\_ maybePiece ->
            Maybe.map (\piece -> piece) maybePiece
        )
        |> List.filterMap identity


-- Get all pieces on the board that belong to a specific player
getPiecesForPlayer : Color -> Board -> List Piece
getPiecesForPlayer color board =
    boardToPieces board
        |> List.filter (\piece -> piece.color == color)


-- Check if a player can still place pieces (max 9 per player)
canPlacePiece : Color -> GameState -> Bool
canPlacePiece color gameState =
    let
        piecesPlaced =
            case color of
                White -> gameState.whitePiecesPlaced
                Black -> gameState.blackPiecesPlaced
    in
    piecesPlaced < 9


-- Place a piece of the given color at the specified position on the board
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


-- Switch to the other player (White -> Black or Black -> White)
nextPlayer : Color -> Color
nextPlayer color =
    case color of
        White -> Black
        Black -> White


-- VIEW

-- Render the game UI: current player display, game board, and reset button
view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-gray-900 flex flex-col items-center justify-center p-8" ]
        [ -- Game header
          div [ class "text-white text-2xl mb-8" ]
            [ text ("Current Player: " ++ playerToString model.gameState.currentPlayer) ]

        , -- Board
          viewBoard model.gameState.selectedPiece (boardToPieces model.board) ClickedPosition ClickedPiece

        , -- Controls
        -- reset button
          button
            [ class "mt-8 bg-blue-600 hover:bg-blue-700 text-white font-bold py-3 px-8 rounded-lg transition-colors"
            , onClick NewGame
            ]
            [ text "Game Reset" ]
        ]


-- MAIN

-- Application entry point - sets up the Elm architecture (init, view, update)
main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
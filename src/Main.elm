module Main exposing (main)

{-| Nine Men's Morris Game - Main Module

This module implements the core game logic for Nine Men's Morris (also known as Mill).

## GAME PHASES
1. Placement Phase: Players take turns placing their 9 pieces on the board
2. Movement Phase: After all pieces are placed, players move pieces to adjacent positions
3. Flying Phase: When a player has only 3 pieces left, they can "fly" to any position (TODO)

## GAME FLOW
- Players alternate turns (White starts)
- In Placement: Click empty position to place a piece
- In Movement: Click your piece to select it, then click adjacent empty position to move
- Forming a mill (3 in a row) allows removing an opponent's piece (TODO)
- Win by reducing opponent to 2 pieces or blocking all their moves (TODO)
-}

import Browser
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Svg exposing (Svg)
import Types exposing (Piece, Color(..), Board, GameState, Position, emptyBoard, initialGameState, GamePhase(..), playerToString)
import View.ViewBoard exposing (viewBoard)
import Board exposing (getPieceAt, getAdjacencies, isPositionEmpty, isMill)


-- MODEL
-- The application state that changes over time

-- The main application state
-- Tracks both the board (where pieces are) and game state (whose turn, what phase, etc.)
type alias Model =
    { board : Board           -- The 24 positions, each with Maybe Piece
    , gameState : GameState   -- Current player, phase, piece counts, selected piece
    }


-- Initialize a new game with an empty board
-- Called when the app starts and when user clicks "Game Reset"
init : Model
init =
    { board = emptyBoard            -- All 24 positions start empty
    , gameState = initialGameState  -- White player, Placement phase, no pieces placed
    }


-- MESSAGES
-- All possible user interactions that can change the model

-- All possible user actions in the game
-- These are sent when the user interacts with the UI
type Msg
    = ClickedPosition Position  -- User clicked an empty board position (to place or move to)
    | ClickedPiece Position     -- User clicked an existing piece (to select for movement)
    | NewGame                   -- User clicked the reset button
    | NoOp                      -- No operation (placeholder for future use)


-- UPDATE

-- Handle user actions and update the game state accordingly
-- This is the core game logic that processes all user interactions
update : Msg -> Model -> Model
update msg model =
    case msg of
        -- HANDLE CLICKING ON AN EXISTING PIECE
        -- This happens when a player clicks on a piece during Movement phase
        ClickedPiece pos ->
            handleMovementClick pos model

        -- HANDLE CLICKING ON AN EMPTY POSITION
        -- This happens in two scenarios:
        -- 1. Placement phase: placing a new piece
        -- 2. Movement phase: moving a selected piece to a new position
        ClickedPosition pos ->
            if model.gameState.phase == Placement then
                handlePlacement pos model
            else if model.gameState.phase == Movement then
                attemptMove pos model
            else
                -- OTHER PHASES (like Flying phase, not yet implemented)
                -- For now, just clear any selection
                let
                    newGameState = model.gameState
                    updatedGameState = { newGameState | selectedPiece = Nothing }
                in
                { model | gameState = updatedGameState }

        -- HANDLE GAME RESET
        -- User clicked the "Game Reset" button
        NewGame ->
            init  -- Reset to initial state

        -- NO OPERATION
        -- Placeholder for future use
        NoOp ->
            model


-- PHASE HANDLERS
-- Dedicated functions for handling different game phases

-- Handle clicking on a piece during Movement phase
-- Allows players to select their own pieces or attempt moves
handleMovementClick : Position -> Model -> Model
handleMovementClick pos model =
    -- Only allow piece selection during Movement phase
    -- (In Placement phase, pieces aren't clicked - positions are)
    if model.gameState.phase == Movement then
        let
            -- Find out what color piece is at this position
            pieceColor = getPieceAt pos model.board
        in
        case pieceColor of
            Just color ->
                -- Check if the piece belongs to the current player
                -- Players can only select their own pieces
                if color == model.gameState.currentPlayer then
                    let
                        -- Select this piece for movement
                        newGameState = model.gameState
                        updatedGameState = { newGameState | selectedPiece = Just pos }
                    in
                    { model | gameState = updatedGameState }
                else
                    -- Clicked opponent's piece - try to move selected piece here
                    -- This handles case where user clicked opponent piece while having a piece selected
                    case model.gameState.selectedPiece of
                        Just selectedPos ->
                            -- Treat this as trying to capture (not yet implemented)
                            -- For now, just deselect
                            let
                                oldGameState = model.gameState
                                updatedGameState = { oldGameState | selectedPiece = Nothing }
                            in
                            { model | gameState = updatedGameState }
                        Nothing ->
                            -- No piece selected, just ignore
                            model
            Nothing ->
                -- No piece at this position - the position is actually empty
                -- This can happen if the view sent ClickedPiece for an empty spot
                -- Treat it as ClickedPosition instead
                case model.gameState.selectedPiece of
                    Just selectedPos ->
                        -- Try to move the selected piece to this empty position
                        -- (Reuse the attemptMove logic)
                        attemptMove pos model
                    Nothing ->
                        -- No piece selected and no piece here, do nothing
                        model
    else
        -- Not in Movement phase - ignore piece clicks
        model


-- Handle placing a piece during Placement phase
-- Places a new piece on the board if valid
handlePlacement : Position -> Model -> Model
handlePlacement pos model =
    let
        currentPlayer = model.gameState.currentPlayer
    in
    -- Verify the position is empty and player hasn't placed all 9 pieces yet
    if getPieceAt pos model.board == Nothing && canPlacePiece currentPlayer model.gameState then
        let
            -- Place the new piece on the board
            newBoard = placePiece pos currentPlayer model.board -- TODO: implement isMill ===================================================================================================================

            -- Update the count of pieces placed for each player
            -- Only increment the count for the current player
            (newWhiteCount, newBlackCount) =
                case currentPlayer of
                    White -> (model.gameState.whitePiecesPlaced + 1, model.gameState.blackPiecesPlaced)
                    Black -> (model.gameState.whitePiecesPlaced, model.gameState.blackPiecesPlaced + 1)

            -- Check if we should transition to Movement phase
            -- This happens when both players have placed all 9 pieces (18 total)
            newPhase =
                if newWhiteCount == 9 && newBlackCount == 9 then
                    Movement  -- All pieces placed, start moving them
                else
                    Placement  -- Still placing pieces

            -- Update game state: switch player and update counts
            newGameState =
                { currentPlayer = nextPlayer currentPlayer
                , selectedPiece = Nothing
                , phase = newPhase
                , whitePiecesPlaced = newWhiteCount
                , blackPiecesPlaced = newBlackCount
                , whitePiecesLeft = model.gameState.whitePiecesLeft
                , blackPiecesLeft = model.gameState.blackPiecesLeft
                }
        in
        { model | board = newBoard, gameState = newGameState }
    else
        -- Invalid placement: position occupied or player has no pieces left
        model


-- Attempt to move a selected piece to a position during Movement phase
-- Validates adjacency and executes the move if valid
attemptMove : Position -> Model -> Model
attemptMove pos model =
    case model.gameState.selectedPiece of
        Just selectedPos ->
            -- A piece is selected - try to move it to the clicked position

            -- First, check if the destination is empty
            if isPositionEmpty pos model.board then
                -- Get the color of the selected piece
                case getPieceAt selectedPos model.board of
                    Just selectedColor ->
                        let
                            -- Create a piece record to get adjacency info
                            selectedPiece = { color = selectedColor, position = selectedPos }
                            -- Get all positions adjacent to the selected piece
                            adjacentPositions = getAdjacencies selectedPiece
                        in
                        -- Check if the destination is adjacent to the current position
                        -- In Movement phase, pieces can only move to adjacent spots
                        if List.member pos adjacentPositions then
                            -- VALID MOVE: destination is adjacent and empty
                            let
                                -- Step 1: Remove piece from its old position
                                boardWithRemoved = removePiece selectedPos model.board
                                -- Step 2: Place piece at the new position
                                newBoard = placePiece pos selectedColor boardWithRemoved -- TODO: implement isMill ===================================================================================================================

                                -- Update game state: switch to next player, clear selection
                                newGameState =
                                    { currentPlayer = nextPlayer model.gameState.currentPlayer
                                    , selectedPiece = Nothing
                                    , phase = Movement
                                    , whitePiecesPlaced = model.gameState.whitePiecesPlaced
                                    , blackPiecesPlaced = model.gameState.blackPiecesPlaced
                                    , whitePiecesLeft = model.gameState.whitePiecesLeft
                                    , blackPiecesLeft = model.gameState.blackPiecesLeft
                                    }
                            in
                            { model | board = newBoard, gameState = newGameState }
                        else
                            -- INVALID: destination is not adjacent to selected piece
                            -- Keep the model as is (piece stays selected)
                            model
                    Nothing ->
                        -- Selected position has no piece - shouldn't happen
                        model
            else
                -- Destination is occupied - can't move there
                model
        Nothing ->
            -- No piece selected - clicking empty position does nothing
            model


-- HELPER FUNCTIONS
-- These functions help with board manipulation and game logic

-- Convert the board (list of Maybe Piece) into a list of actual pieces
-- Filters out empty positions (Nothing values) and returns only actual pieces
-- Example: [Nothing, Just piece1, Nothing, Just piece2] -> [piece1, piece2]
boardToPieces : Board -> List Piece
boardToPieces board =
    board
        |> List.indexedMap (\_ maybePiece ->
            Maybe.map (\piece -> piece) maybePiece
        )
        |> List.filterMap identity  -- Remove Nothing values


-- Get all pieces on the board that belong to a specific player
-- Useful for counting pieces or checking game state
-- Example: getPiecesForPlayer White board -> [all white pieces]
getPiecesForPlayer : Color -> Board -> List Piece
getPiecesForPlayer color board =
    boardToPieces board
        |> List.filter (\piece -> piece.color == color)


-- Check if a player can still place pieces during Placement phase
-- Each player can place a maximum of 9 pieces
-- Returns True if player has placed fewer than 9 pieces
canPlacePiece : Color -> GameState -> Bool
canPlacePiece color gameState =
    let
        -- Get the count of pieces already placed by this player
        piecesPlaced =
            case color of
                White -> gameState.whitePiecesPlaced
                Black -> gameState.blackPiecesPlaced
    in
    piecesPlaced < 9  -- Can place if fewer than 9 pieces on board


-- Place a piece of the given color at the specified position on the board
-- Creates a new piece at the given position with the specified color
-- Used in both Placement phase (placing new pieces) and Movement phase (moving pieces)
-- Example: placePiece 5 White board -> board with white piece at position 5
placePiece : Int -> Color -> Board -> Board
placePiece pos color board =
    List.indexedMap
        (\index maybePiece ->
            if index == pos then
                -- Create a new piece at this position
                Just { color = color, position = pos }
            else
                -- Keep the existing piece (or Nothing) at other positions
                maybePiece
        )
        board


-- Remove a piece from the specified position on the board
-- Sets the position to Nothing (empty)
-- Used when moving a piece (remove from old position before placing at new one)
-- Also used when capturing opponent's pieces (not yet implemented)
-- Example: removePiece 5 board -> board with position 5 now empty
removePiece : Int -> Board -> Board
removePiece pos board =
    List.indexedMap
        (\index maybePiece ->
            if index == pos then
                Nothing  -- Clear this position
            else
                maybePiece  -- Keep other positions unchanged
        )
        board

handleFlyingClick : Int -> Piece -> Board -> Board
handleFlyingClick pos piece board =
    if getPieceAt pos board == Nothing then
        placePiece pos piece.color board
        |> removePiece piece.position board
    else
        board

-- Switch to the other player (White -> Black or Black -> White)
-- Called after each successful turn to alternate players
nextPlayer : Color -> Color
nextPlayer color =
    case color of
        White -> Black  -- If White just played, Black plays next
        Black -> White  -- If Black just played, White plays next


-- VIEW
-- Renders the user interface

-- Render the game UI: current player display, game board, and reset button
-- This is called automatically by Elm whenever the model changes
view : Model -> Html Msg
view model =
    div [ class "min-h-screen bg-gray-900 flex flex-col items-center justify-center p-8" ]
        [ -- GAME HEADER
          -- Shows which player's turn it is
          div [ class "text-white text-2xl mb-8" ]
            [ text ("Current Player: " ++ playerToString model.gameState.currentPlayer) ]

        , -- GAME BOARD
          -- Renders the actual Nine Men's Morris board with all pieces
          -- Passes the selected piece (if any) for highlighting
          -- Passes callback functions for when user clicks positions or pieces
          viewBoard model.gameState.selectedPiece (boardToPieces model.board) ClickedPosition ClickedPiece

        , -- GAME CONTROLS
          -- Reset button to start a new game
          button
            [ class "mt-8 bg-blue-600 hover:bg-blue-700 text-white font-bold py-3 px-8 rounded-lg transition-colors"
            , onClick NewGame
            ]
            [ text "Game Reset" ]
        ]


-- MAIN
-- Application entry point

-- Sets up the Elm architecture with Model-View-Update pattern
-- Browser.sandbox is used for simple apps without side effects
main : Program () Model Msg
main =
    Browser.sandbox
        { init = init       -- Initial model state
        , view = view       -- How to render the model to HTML
        , update = update   -- How to update the model based on messages
        }
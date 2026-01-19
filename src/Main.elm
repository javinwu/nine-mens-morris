module Main exposing (main)

{-| Nine Men's Morris Game

GAME PHASES:
1. Placement: Players place 9 pieces each on empty positions
   - Mill formed → REMOVING phase (must remove opponent piece)
   - Both placed all 9 → MOVEMENT phase

2. Movement: Players move pieces to adjacent positions
   - Mill formed → REMOVING phase
   - Player reaches 3 pieces → FLYING phase

3. Flying: Player with 3 pieces can move to ANY empty position
   - Mill formed → REMOVING phase

4. Removing: Current player MUST remove an opponent's piece
   - Can remove any piece NOT in a mill
   - If ALL opponent pieces are in mills, can remove any piece
   - After removal → Return to previous phase
-}

import Browser
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Svg exposing (Svg)
import Maybe.Extra exposing (isNothing)
import Types exposing (Piece, Color(..), Board, GameState, Position, emptyBoard, initialGameState, GamePhase(..), playerToString)
import View.ViewBoard exposing (viewBoard)
import Board exposing (getPieceAt, getAdjacencies, isPositionEmpty, isMill)


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


-- ============================================================================
-- UPDATE
-- ============================================================================

update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedPiece pos ->
            if model.gameState.phase == Removing then
                handleRemovePiece pos model
            else
                handleMovementClick pos model

        ClickedPosition pos ->
            if model.gameState.phase == Placement then
                handlePlacement pos model
            else if model.gameState.phase == Movement then
                attemptMove pos model
            else if model.gameState.phase == Flying then
                attemptMove pos model
            else if model.gameState.phase == Removing then
                model
            else
                let
                    gs = model.gameState
                in
                { model | gameState = { gs | selectedPiece = Nothing } }

        NewGame ->
            init

        NoOp ->
            model


-- ============================================================================
-- PHASE HANDLERS
-- ============================================================================

handleMovementClick : Position -> Model -> Model
handleMovementClick pos model =
    if model.gameState.phase == Movement || model.gameState.phase == Flying then
        case getPieceAt pos model.board of
            Just color ->
                if color == model.gameState.currentPlayer then
                    let
                        gs = model.gameState
                    in
                    { model | gameState = { gs | selectedPiece = Just pos } }
                else
                    case model.gameState.selectedPiece of
                        Just _ ->
                            let
                                gs = model.gameState
                            in
                            { model | gameState = { gs | selectedPiece = Nothing } }
                        Nothing ->
                            model
            Nothing ->
                case model.gameState.selectedPiece of
                    Just _ ->
                        attemptMove pos model
                    Nothing ->
                        model
    else
        model


handlePlacement : Position -> Model -> Model
handlePlacement pos model =
    let
        currentPlayer = model.gameState.currentPlayer
    in
    if getPieceAt pos model.board == Nothing && canPlacePiece currentPlayer model.gameState then
        let
            newBoard = placePiece pos currentPlayer model.board
            placedPiece = { color = currentPlayer, position = pos }
            formedMill = isMill placedPiece newBoard

            (newWhiteCount, newBlackCount) =
                case currentPlayer of
                    White -> (model.gameState.whitePiecesPlaced + 1, model.gameState.blackPiecesPlaced)
                    Black -> (model.gameState.whitePiecesPlaced, model.gameState.blackPiecesPlaced + 1)

            basePhase =
                if newWhiteCount == 9 && newBlackCount == 9 then
                    Movement
                else
                    Placement

            (newPhase, nextPhaseAfterRemove) =
                if formedMill then
                    (Removing, Just basePhase)
                else
                    (basePhase, Nothing)

            newGameState =
                { currentPlayer = if formedMill then currentPlayer else nextPlayer currentPlayer
                , selectedPiece = Nothing
                , phase = newPhase
                , whitePiecesPlaced = newWhiteCount
                , blackPiecesPlaced = newBlackCount
                , whitePiecesLeft = model.gameState.whitePiecesLeft
                , blackPiecesLeft = model.gameState.blackPiecesLeft
                , nextPhaseAfterRemove = nextPhaseAfterRemove
                }
        in
        { model | board = newBoard, gameState = newGameState }
    else
        model


-- Returns Flying if player has 3 pieces, otherwise Movement
determinePhaseForPlayer : Color -> GameState -> GamePhase
determinePhaseForPlayer color gameState =
    let
        piecesLeft =
            case color of
                White -> gameState.whitePiecesLeft
                Black -> gameState.blackPiecesLeft
    in
    if piecesLeft == 3 then
        Flying
    else
        Movement


attemptMove : Position -> Model -> Model
attemptMove pos model =
    case model.gameState.selectedPiece of
        Just selectedPos ->
            if isPositionEmpty pos model.board then
                case getPieceAt selectedPos model.board of
                    Just selectedColor ->
                        let
                            selectedPiece = { color = selectedColor, position = selectedPos }
                            adjacentPositions = getAdjacencies selectedPiece

                            isValidMove =
                                if model.gameState.phase == Flying then
                                    True
                                else
                                    List.member pos adjacentPositions
                        in
                        if isValidMove then
                            let
                                boardWithRemoved = removePiece selectedPos model.board
                                newBoard = placePiece pos selectedColor boardWithRemoved
                                movedPiece = { color = selectedColor, position = pos }
                                formedMill = isMill movedPiece newBoard
                                basePhase = determinePhaseForPlayer model.gameState.currentPlayer model.gameState

                                (newPhase, nextPhaseAfterRemove) =
                                    if formedMill then
                                        (Removing, Just basePhase)
                                    else
                                        (determinePhaseForPlayer (nextPlayer model.gameState.currentPlayer) model.gameState, Nothing)

                                newGameState =
                                    { currentPlayer = if formedMill then model.gameState.currentPlayer else nextPlayer model.gameState.currentPlayer
                                    , selectedPiece = Nothing
                                    , phase = newPhase
                                    , whitePiecesPlaced = model.gameState.whitePiecesPlaced
                                    , blackPiecesPlaced = model.gameState.blackPiecesPlaced
                                    , whitePiecesLeft = model.gameState.whitePiecesLeft
                                    , blackPiecesLeft = model.gameState.blackPiecesLeft
                                    , nextPhaseAfterRemove = nextPhaseAfterRemove
                                    }
                            in
                            { model | board = newBoard, gameState = newGameState }
                        else
                            model
                    Nothing ->
                        model
            else
                model
        Nothing ->
            model


-- ============================================================================
-- REMOVING - Handle piece removal after forming a mill
-- ============================================================================

handleRemovePiece : Position -> Model -> Model
handleRemovePiece pos model =
    let
        pieceColor = getPieceAt pos model.board
        opponentColor = nextPlayer model.gameState.currentPlayer
    in
    case pieceColor of
        Just color ->
            if color == opponentColor then
                if canRemovePiece pos color model.board then
                    let
                        newBoard = removePiece pos model.board

                        (newWhiteLeft, newBlackLeft) =
                            case color of
                                White -> (model.gameState.whitePiecesLeft - 1, model.gameState.blackPiecesLeft)
                                Black -> (model.gameState.whitePiecesLeft, model.gameState.blackPiecesLeft - 1)

                        tempGameState =
                            { currentPlayer = opponentColor
                            , selectedPiece = Nothing
                            , phase = model.gameState.phase
                            , whitePiecesPlaced = model.gameState.whitePiecesPlaced
                            , blackPiecesPlaced = model.gameState.blackPiecesPlaced
                            , whitePiecesLeft = newWhiteLeft
                            , blackPiecesLeft = newBlackLeft
                            , nextPhaseAfterRemove = Nothing
                            }

                        basePhase = Maybe.withDefault Movement model.gameState.nextPhaseAfterRemove
                        newPhase =
                            if basePhase == Placement then
                                Placement
                            else
                                determinePhaseForPlayer opponentColor tempGameState

                        newGameState =
                            { tempGameState | phase = newPhase }
                    in
                    { model | board = newBoard, gameState = newGameState }
                else
                    model
            else
                model
        Nothing ->
            model


-- ============================================================================
-- HELPER FUNCTIONS
-- ============================================================================

boardToPieces : Board -> List Piece
boardToPieces board =
    board
        |> List.indexedMap (\_ maybePiece -> Maybe.map (\piece -> piece) maybePiece)
        |> List.filterMap identity


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


removePiece : Int -> Board -> Board
removePiece pos board =
    List.indexedMap
        (\index maybePiece ->
            if index == pos then
                Nothing
            else
                maybePiece
        )
        board


handleFlyingClick : Int -> Piece -> Board -> Board
handleFlyingClick pos piece board =
    if getPieceAt pos board == Nothing then
        placePiece pos piece.color board
        |> removePiece piece.position
    else
        board


-- Can remove if: not in mill OR all opponent pieces are in mills
canRemovePiece : Position -> Color -> Board -> Bool
canRemovePiece pos color board =
    let
        piece = { color = color, position = pos }
        pieceInMill = isMill piece board
    in
    if not pieceInMill then
        True
    else
        let
            opponentPieces = getPiecesForPlayer color board
            allInMills = List.all (\p -> isMill p board) opponentPieces
        in
        allInMills


nextPlayer : Color -> Color
nextPlayer color =
    case color of
        White -> Black
        Black -> White

checkWin : Board -> Color -> Model -> Bool
checkWin board color model =
    if color == White then
        if model.gameState.whitePiecesLeft <= 2 then
            True
        else
            let
                pieces = getPiecesForPlayer color board
            in
            List.map getPieceAt (List.concatMap getAdjacencies pieces)
            |> List.map (\getPiece -> getPiece board)
            |> List.all isNothing
            |> not
    else
        if model.gameState.blackPiecesLeft <= 2 then
            True
        else
            let
                pieces = getPiecesForPlayer color board
            in
            List.map getPieceAt (List.concatMap getAdjacencies pieces)
            |> List.map (\getPiece -> getPiece board)
            |> List.all isNothing
            |> not

-- ============================================================================
-- VIEW
-- ============================================================================

getPhaseMessage : GamePhase -> String
getPhaseMessage phase =
    case phase of
        Placement -> "Place your pieces on the board"
        Movement -> "Move your piece to an adjacent position"
        Flying -> "Fly your piece to any empty position"
        Removing -> "Mill formed! Remove an opponent's piece"


view : Model -> Html Msg
view model =
    div [ class "min-h-screen flex flex-col items-center justify-center p-4" ]
        [ div [ class "flex flex-col items-center justify-center w-full max-w-2xl mx-auto" ]
            [ div [ class "text-center mb-4" ]
                [ div [ class "text-white text-xl mb-1" ]
                    [ text ("Current Player: " ++ playerToString model.gameState.currentPlayer) ]
                , div [ class "text-gray-300 text-base" ]
                    [ text (getPhaseMessage model.gameState.phase) ]
                ]
            , div [ class "w-full max-w-lg mx-auto" ]
                [ viewBoard model.gameState.selectedPiece (boardToPieces model.board) ClickedPosition ClickedPiece ]
            , button
                [ class "mt-4 bg-blue-600 hover:bg-blue-700 text-white font-bold py-2 px-6 rounded-lg transition-colors"
                , onClick NewGame
                ]
                [ text "Game Reset" ]
            ]
        ]


-- MAIN

main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }

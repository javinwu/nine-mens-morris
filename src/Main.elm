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

5. GameOver: Game is over and a winner is declared.
    - Cannot make any actions other than reset board
-}

import Browser
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)
import Time
--import Svg exposing (Svg) ==========================================================================================================================================================================================================================================================================================================
import List exposing (length)
import Maybe.Extra exposing (isNothing)
import Types exposing (Piece, Color(..), Board, GameState, Position, emptyBoard, initialGameState, GamePhase(..), playerToString)
import View.ViewBoard exposing (viewBoard)
import View.UI exposing (nextGameButton)
import Board exposing (getPieceAt, getAdjacencies, isPositionEmpty, isMill, getMillPositions)


-- MODEL

type alias Model =
    { board : Board
    , gameState : GameState
    , elapsedTime : Int
    , timerRunning : Bool
    }


init :() -> ( Model, Cmd Msg )
init _ =
    ( { board = emptyBoard
      , gameState = initialGameState
      , elapsedTime = 0
      , timerRunning = False
      }
    , Cmd.none
    )


-- MESSAGES

type Msg
    = ClickedPosition Position
    | ClickedPiece Position
    | NewGame
    | NoOp
    | Tick Time.Posix



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            if model.timerRunning then
                ( { model | elapsedTime = model.elapsedTime + 1 }, Cmd.none )
            else
                ( model, Cmd.none )

        ClickedPiece pos ->
            let
                updatedModel =
                    if model.gameState.phase == Removing then
                        handleRemovePiece pos model
                    else
                        handleMovementClick pos model
            in
            ( checkAndUpdateTimer updatedModel, Cmd.none )
        
        ClickedPosition pos ->
            let
                updatedModel =
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
                        { model | gameState = { gs | selectedPiece = Nothing, validMovePositions = [], millPositions = [] } }
            in
            ( checkAndUpdateTimer updatedModel, Cmd.none )

        NewGame ->
            init ()

        NoOp ->
            ( model, Cmd.none )


-- Check if timer should start or stop based on game state
checkAndUpdateTimer : Model -> Model
checkAndUpdateTimer model =
    let
        hasPlacedPieces = model.gameState.whitePiecesPlaced > 0 || model.gameState.blackPiecesPlaced > 0
        shouldRun = hasPlacedPieces && model.gameState.phase /= GameOver
    in
    { model | timerRunning = shouldRun }


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.timerRunning then
        Time.every 1000 Tick
    else
        Sub.none


-- PHASE HANDLERS


handleMovementClick : Position -> Model -> Model
handleMovementClick pos model =
    if model.gameState.phase == Movement || model.gameState.phase == Flying then
        case getPieceAt pos model.board of
            Just color ->
                if color == model.gameState.currentPlayer then
                    let
                        gs = model.gameState
                        validPositions = getValidMovePositions pos model.gameState.phase model.board
                    in
                    { model | gameState = { gs | selectedPiece = Just pos, validMovePositions = validPositions, millPositions = [] } }
                else
                    case model.gameState.selectedPiece of
                        Just _ ->
                            let
                                gs = model.gameState
                            in
                            { model | gameState = { gs | selectedPiece = Nothing, validMovePositions = [], millPositions = [] } }
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
            millPos = if formedMill then getMillPositions placedPiece newBoard else []

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
                , validMovePositions = []
                , millPositions = millPos
                }
        in
        { model | board = newBoard, gameState = newGameState }
    else
        model


-- Returns GameOver if game is over, Flying if player has 3 pieces, otherwise Movement
determinePhaseForPlayer : Color -> Board -> GameState -> GamePhase
determinePhaseForPlayer color board gameState =
    let
        piecesLeft =
            case color of
                White -> gameState.whitePiecesLeft
                Black -> gameState.blackPiecesLeft
    in
    if checkWin board then
        GameOver
    else if piecesLeft == 3 then
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
                                millPos = if formedMill then getMillPositions movedPiece newBoard else []
                                basePhase = determinePhaseForPlayer model.gameState.currentPlayer newBoard model.gameState

                                (newPhase, nextPhaseAfterRemove) =
                                    if formedMill then
                                        (Removing, Just basePhase)
                                    else
                                        (determinePhaseForPlayer (nextPlayer model.gameState.currentPlayer) newBoard model.gameState, Nothing)

                                newGameState =
                                    { currentPlayer = if formedMill then model.gameState.currentPlayer else nextPlayer model.gameState.currentPlayer
                                    , selectedPiece = Nothing
                                    , phase = newPhase
                                    , whitePiecesPlaced = model.gameState.whitePiecesPlaced
                                    , blackPiecesPlaced = model.gameState.blackPiecesPlaced
                                    , whitePiecesLeft = model.gameState.whitePiecesLeft
                                    , blackPiecesLeft = model.gameState.blackPiecesLeft
                                    , nextPhaseAfterRemove = nextPhaseAfterRemove
                                    , validMovePositions = []
                                    , millPositions = millPos
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



-- REMOVING - Handle piece removal after forming a mill

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
                            , validMovePositions = []
                            , millPositions = []
                            }

                        basePhase = Maybe.withDefault Movement model.gameState.nextPhaseAfterRemove
                        newPhase =
                            if basePhase == Placement then
                                Placement
                            else
                                determinePhaseForPlayer opponentColor newBoard tempGameState

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



-- HELPER FUNCTIONS

{-| Calculate valid move positions for a selected piece
For Movement phase: returns empty adjacent positions
For Flying phase: returns all empty positions on the board
-}
getValidMovePositions : Position -> GamePhase -> Board -> List Position
getValidMovePositions selectedPos phase board =
    case getPieceAt selectedPos board of
        Just _ ->
            let
                emptyPositions =
                    List.range 0 23
                        |> List.filter (\pos -> isPositionEmpty pos board)
            in
            if phase == Flying then
                -- Flying: can move to any empty position
                emptyPositions
            else if phase == Movement then
                -- Movement: can only move to adjacent empty positions
                let
                    selectedPiece = { color = White, position = selectedPos }  -- color doesn't matter for adjacency
                    adjacentPositions = getAdjacencies selectedPiece
                in
                List.filter (\pos -> List.member pos adjacentPositions) emptyPositions
            else
                []
        Nothing ->
            []


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

checkWin : Board -> Bool
checkWin board =
    if length (getPiecesForPlayer White board) <= 2 || length(getPiecesForPlayer Black board) <= 2 then --if one of the players has two pieces, game over
        True
    else
        let
            whitePieces = getPiecesForPlayer White board
            blackPieces = getPiecesForPlayer Black board
        in
        if List.all isNothing (List.map (\getPiece -> getPiece board) (List.map getPieceAt (List.concatMap getAdjacencies whitePieces))) then   -- if white still has moves, check black
            List.concatMap getAdjacencies blackPieces
            |> List.map getPieceAt
            |> List.map (\getPiece -> getPiece board)
            |> List.all isNothing
            |> not
        else
            False


-- Format time as HH:MM:SS
formatTime : Int -> String
formatTime totalSeconds =
    let
        seconds = modBy 60 totalSeconds
        minutes = totalSeconds // 60
        hours = minutes // 60
    in
    String.padLeft 2 '0' (String.fromInt hours)
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt minutes)
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt seconds)


-- VIEW

getPhaseMessage : GamePhase -> String
getPhaseMessage phase =
    case phase of
        Placement -> "Place your pieces on the board"
        Movement -> "Move your piece to an adjacent position"
        Flying -> "Fly your piece to any empty position"
        Removing -> "Mill formed! Remove an opponent's piece"
        GameOver -> "Game over! Reset the board to play again."


{-| Get piece counts for display
Returns (on board, in hand) for the given color
-}
getPieceCounts : Color -> GameState -> Board -> (Int, Int)
getPieceCounts color gameState board =
    let
        onBoard = List.length (getPiecesForPlayer color board)
        placed = case color of
            White -> gameState.whitePiecesPlaced
            Black -> gameState.blackPiecesPlaced
        inHand = 9 - placed
    in
    (onBoard, inHand)


viewPieceCount : Color -> GameState -> Board -> Html Msg
viewPieceCount color gameState board =
    let
        (onBoard, inHand) = getPieceCounts color gameState board
        colorName = playerToString color
        bgColor = case color of
            White -> "bg-gray-100"
            Black -> "bg-gray-800"
        textColor = case color of
            White -> "text-gray-900"
            Black -> "text-white"
        borderColor =
            if gameState.currentPlayer == color then
                "border-yellow-400 border-4"
            else
                "border-gray-500 border-2"
    in
    div [ class ("px-4 py-3 rounded-lg " ++ bgColor ++ " " ++ textColor ++ " " ++ borderColor) ]
        [ div [ class "font-bold text-2xl mb-1" ] [ text colorName ]
        , div [ class "text-lg" ] [ text ("On board: " ++ String.fromInt onBoard) ]
        , if gameState.phase == Placement then
            div [ class "text-lg" ] [ text ("In hand: " ++ String.fromInt inHand) ]
          else
            text ""
        ]


view : Model -> Html Msg
view model =
    div [ class "min-h-screen flex flex-col items-center justify-center p-4" ]
        [ div [ class "flex flex-col items-center justify-center w-full max-w-2xl mx-auto" ]
            [ div [ class "text-center mb-4 h-24 flex flex-col justify-center" ]
                [ div [ class "text-white text-3xl mb-1" ]
                    [ text (formatTime model.elapsedTime) ]
                , div [ class "text-white text-xl mb-1" ]
                    [ text ("Current Player: " ++ playerToString model.gameState.currentPlayer) ]
                , div [ class "text-gray-300 text-xl min-h-[2rem]" ]
                    [ text (getPhaseMessage model.gameState.phase) ]
                ]
            , div [ class "flex justify-center gap-4 mb-4 w-full max-w-lg" ]
                [ viewPieceCount Black model.gameState model.board
                , viewPieceCount White model.gameState model.board
                ]
            , div [ class "w-full max-w-lg mx-auto" ]
                [ viewBoard model.gameState.selectedPiece model.gameState.validMovePositions model.gameState.millPositions (boardToPieces model.board) ClickedPosition ClickedPiece ]
            , nextGameButton NewGame
            ]
        ]


-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

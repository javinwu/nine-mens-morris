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
import List exposing (length)
import Array
import Maybe.Extra exposing (isNothing)
import Process
import Task
import Types exposing (Piece, Color(..), Board, GameState, Position, emptyBoard, initialGameState, GamePhase(..), playerToString)
import View.ViewBoard exposing (viewBoard)
import View.UI exposing (nextGameButton)
import Board exposing (getPieceAt, getAdjacencies, isPositionEmpty, isMill, getMillPositions, getAllMillPositions)
import Sounds exposing (playPlaceSound)

type alias Model =
    { board : Board
    , gameState : GameState
    , elapsedTime : Int
    , timerRunning : Bool
    , showMillNotification : Bool
    }


init :() -> ( Model, Cmd Msg )
init _ =
    ( { board = emptyBoard
      , gameState = initialGameState
      , elapsedTime = 0
      , timerRunning = False
      , showMillNotification = False
      }
    , Cmd.none
    )

type Msg
    = ClickedPosition Position
    | ClickedPiece Position
    | NewGame
    | NoOp
    | Tick Time.Posix
    | HideMillNotification

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

                millNotificationCmd =
                    if updatedModel.showMillNotification && not model.showMillNotification then
                        Process.sleep 2000
                            |> Task.perform (\_ -> HideMillNotification)
                    else
                        Cmd.none
            in
            ( checkAndUpdateTimer updatedModel, millNotificationCmd )
        
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
                        { model | gameState = { gs | selectedPiece = Nothing, validMovePositions = [] } }

                -- Play sound if a piece was placed (board changed)
                soundCmd =
                    if model.gameState.phase == Placement && model.board /= updatedModel.board then
                        playPlaceSound
                    else
                        Cmd.none

                millNotificationCmd =
                    if updatedModel.showMillNotification && not model.showMillNotification then
                        Process.sleep 2000
                            |> Task.perform (\_ -> HideMillNotification)
                    else
                        Cmd.none
            in
            ( checkAndUpdateTimer updatedModel, Cmd.batch [ soundCmd, millNotificationCmd ] )

        NewGame ->
            init ()

        NoOp ->
            ( model, Cmd.none )

        HideMillNotification ->
            ( { model | showMillNotification = False }, Cmd.none )

checkAndUpdateTimer : Model -> Model
checkAndUpdateTimer model =
    let
        hasPlacedPieces = model.gameState.whitePiecesPlaced > 0 || model.gameState.blackPiecesPlaced > 0
        shouldRun = hasPlacedPieces && model.gameState.phase /= GameOver
    in
    { model | timerRunning = shouldRun }

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.timerRunning then
        Time.every 1000 Tick
    else
        Sub.none

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
                    { model | gameState = { gs | selectedPiece = Just pos, validMovePositions = validPositions } }
                else
                    case model.gameState.selectedPiece of
                        Just _ ->
                            let
                                gs = model.gameState
                            in
                            { model | gameState = { gs | selectedPiece = Nothing, validMovePositions = [] } }
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

            (newWhiteLeft, newBlackLeft) =
                case currentPlayer of
                    White -> (model.gameState.whitePiecesLeft + 1, model.gameState.blackPiecesLeft)
                    Black -> (model.gameState.whitePiecesLeft, model.gameState.blackPiecesLeft + 1)

            basePhase =
                if newWhiteCount == 9 && newBlackCount == 9 then
                    let
                        nextPlayerColor = nextPlayer currentPlayer
                        tempGameState =
                            { currentPlayer = nextPlayerColor
                            , selectedPiece = Nothing
                            , phase = Movement
                            , whitePiecesPlaced = newWhiteCount
                            , blackPiecesPlaced = newBlackCount
                            , whitePiecesLeft = newWhiteLeft
                            , blackPiecesLeft = newBlackLeft
                            , nextPhaseAfterRemove = Nothing
                            , validMovePositions = []
                            , millPositions = getAllMillPositions newBoard
                            }
                    in
                    determinePhaseForPlayer nextPlayerColor newBoard tempGameState
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
                , whitePiecesLeft = newWhiteLeft
                , blackPiecesLeft = newBlackLeft
                , nextPhaseAfterRemove = nextPhaseAfterRemove
                , validMovePositions = []
                , millPositions = getAllMillPositions newBoard
                }
        in
        { model | board = newBoard, gameState = newGameState, showMillNotification = formedMill }
    else
        model

determinePhaseForPlayer : Color -> Board -> GameState -> GamePhase
determinePhaseForPlayer color board gameState =
    let
        piecesLeft =
            case color of
                White -> gameState.whitePiecesLeft
                Black -> gameState.blackPiecesLeft
    in
    if checkWin color board then
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
                                    , millPositions = getAllMillPositions newBoard
                                    }
                            in
                            { model | board = newBoard, gameState = newGameState, showMillNotification = formedMill }
                        else
                            model
                    Nothing ->
                        model
            else
                model
        Nothing ->
            model

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
                            , millPositions = getAllMillPositions newBoard
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
                emptyPositions
            else if phase == Movement then
                let
                    selectedPiece = { color = White, position = selectedPos }
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
        |> Array.toList
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
    Array.set pos (Just { color = color, position = pos }) board


removePiece : Int -> Board -> Board
removePiece pos board =
    Array.set pos Nothing board

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

hasValidMoves : Color -> Board -> Bool
hasValidMoves color board =
    let
        playerPieces = getPiecesForPlayer color board
        piecesCount = List.length playerPieces
    in
    if piecesCount == 3 then
        List.any (\pos -> isPositionEmpty pos board) (List.range 0 23)
    else
        List.any
            (\piece ->
                let
                    adjacentPositions = getAdjacencies piece
                in
                List.any (\pos -> isPositionEmpty pos board) adjacentPositions
            )
            playerPieces


checkWin : Color -> Board -> Bool
checkWin currentPlayer board =
    let
        playerPieces = getPiecesForPlayer currentPlayer board
        playerCount = List.length playerPieces
    in
    if playerCount <= 2 then
        True
    else if not (hasValidMoves currentPlayer board) then
        True
    else
        False

formatTime : Int -> String
formatTime totalSeconds =
    let
        seconds = modBy 60 totalSeconds
        minutes = totalSeconds // 60
    in
    String.padLeft 2 '0' (String.fromInt minutes)
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt seconds)

getPhaseMessage : GamePhase -> String
getPhaseMessage phase =
    case phase of
        Placement -> "Place your pieces on the board"
        Movement -> "Move your piece to an adjacent position"
        Flying -> "Fly your piece to any empty position"
        Removing -> "Mill formed! Remove an opponent's piece"
        GameOver -> "Game over! Reset the board to play again."

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


getWinner : Board -> String
getWinner board =
    let
        whitePieces = List.length (getPiecesForPlayer White board)
        blackPieces = List.length (getPiecesForPlayer Black board)
        whiteHasMoves = hasValidMoves White board
        blackHasMoves = hasValidMoves Black board
    in
    if whitePieces <= 2 || not whiteHasMoves then
        playerToString Black
    else if blackPieces <= 2 || not blackHasMoves then
        playerToString White
    else
        ""


viewMillNotification : Html Msg
viewMillNotification =
    div [ class "mill-notification" ]
        [ text "MILL FORMED!" ]


viewGameOverScreen : Board -> Html Msg
viewGameOverScreen board =
    let
        winner = getWinner board
        winnerText = if winner /= "" then winner ++ " Wins!" else "Game Over"
    in
    div
        [ class "fixed inset-0 flex flex-col items-center justify-center z-50"
        , Html.Attributes.style "background-color" "#3e2723"
        ]
        [ div [ class "text-center" ]
            [ div [ class "text-8xl font-bold text-amber-100 mb-8" ]
                [ text "GAME OVER" ]
            , div [ class "text-5xl font-bold text-amber-200 mb-12" ]
                [ text winnerText ]
            , nextGameButton NewGame
            ]
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
        , if model.gameState.phase == GameOver then
            viewGameOverScreen model.board
          else
            text ""
        , if model.showMillNotification then
            viewMillNotification
          else
            text ""
        ]

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

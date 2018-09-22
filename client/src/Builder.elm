module Builder exposing (Flags, Frame, Hints(..), Lesson, Model, Msg(..), PartialFenState, Piece(..), Placement, SquareKey, SquareLocation, State, Store(..), SupportedMode(..), Validation(..), alreadyPlacedMaximum, api, conditionalUpdatedBoard, currentTotal, decodeFen, doesntLeadToBalancedGame, encodeFrame, encodeStore, fetchSeedCompleted, findConfig, findNextSeed, findPointValueFromPiece, findStorageKey, generate, generatePlacement, getFrame, getFrameHints, handleGameUpdate, init, is, isLegal, loadingButtonAttributes, main, makeSlider, maximumPieceCount, monarchAlreadyPlaced, monarchsNotAdjacent, notEnoughPointsRemaining, notTooManyPawns, parseInt, pawnsNotInEndRows, piece, pieceGenerator, placement, postLessonCmd, postLessonCompleted, pplacements, radio, saveDefaultMessage, saveHint, sendPlacements, square, squareGenerator, squareNotOpen, subscriptions, team, teamGenerator, toCommand, update, view, viewActionMenu, viewAlerts, viewHints, viewModeSelection, viewNavbar, viewSquareHints)

import AppColor exposing (palette)
import Arithmetic exposing (isEven)
import BuilderJs
import Char
import Chess exposing (Msg, State, fromFen, getSquaresSelected, subscriptions, update, view)
import Chess.Data.Board exposing (Square(..))
import Chess.Data.Piece exposing (Piece(..))
import Chess.Data.Player exposing (Player(..))
import Chess.Data.Position exposing (Position, toRowColumn)
import Chess.View.Board
import Dict exposing (Dict)
import Drag
import Html exposing (Html, a, button, div, fieldset, h1, h2, h3, img, input, label, nav, section, span)
import Html.Attributes as H exposing (defaultValue, href, max, min, src, target, type_)
import Html.Events exposing (on, onClick, targetValue)
import Http exposing (Request, getString, jsonBody)
import Json.Decode as D
import Json.Decode.Pipeline as JDP
import Json.Encode as E
import List.Extra as List
import Mouse
import Piece
import Random.Pcg as Random
import Svg exposing (Svg, g, rect, svg, text, text_)
import Svg.Attributes exposing (fill, fontSize, height, rx, ry, style, viewBox, width, x, y)
import Svg.Events exposing (onMouseDown, onMouseMove, onMouseUp)
import Task
import Time



---- MODEL ----


type Piece
    = Monarch
    | Hand
    | Rook
    | Bishop
    | Knight
    | Pawn


type alias SquareLocation =
    { x : Int, y : Int }


type alias Placement =
    { square : SquareLocation
    , piece : Piece
    , team : Player
    }


type SupportedMode
    = Basic
    | ForcingMoves


type alias Model =
    { apiEndpoint : String
    , mode : SupportedMode
    , store : Store
    , currentGameState : String
    , initialGameState : Maybe String
    , initialSeed : Random.Seed
    , placements : List Placement
    , pointsAllowed : Int
    , submitting : Bool
    , alerts : List String
    , chessModel : Chess.State
    }


type alias Flags =
    { apiEndpoint : String
    , initialSeed : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        initialSeed =
            Random.initialSeed flags.initialSeed

        initialPlacements =
            generate [] 1 initialSeed

        blankGameFen =
            "8/8/8/8/8/8/8/8 w - - 0 0"

        initialChessState =
            case Chess.fromFen blankGameFen of
                Nothing ->
                    Debug.crash "FEN PARSER IS BROKEN, PANIC"

                Just state ->
                    state
    in
    ( { apiEndpoint = flags.apiEndpoint
      , mode = Basic
      , store = Store Dict.empty
      , currentGameState = blankGameFen
      , initialGameState = Nothing
      , initialSeed = initialSeed
      , placements = initialPlacements
      , pointsAllowed = 1
      , submitting = False
      , alerts = []
      , chessModel = initialChessState
      }
    , sendPlacements initialPlacements
    )



-- TODO: MAKE DYNAMIC LATER


findConfig : Chess.View.Board.Config
findConfig =
    { each = "5em", between = "0.15em", borderSize = "0.3em" }



---- UPDATE ----


type Msg
    = Validate
    | HandleGameUpdate String
    | HandleSliderChange Int
    | SelectMode SupportedMode
    | SubmitLesson
    | GetSeed
    | FetchSeedCompleted (Result Http.Error Int)
    | PostLessonCompleted (Result Http.Error String)
    | Record
    | ChessMsg Chess.Msg
    | WriteHint String String
    | WriteDefaultMessage String
    | RegenerateSeed Time.Time


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Validate ->
            ( model, sendPlacements model.placements )

        HandleGameUpdate fen ->
            ( handleGameUpdate model fen, Cmd.none )

        HandleSliderChange pointsAllowed ->
            let
                updatedPlacements =
                    generate [] pointsAllowed (Random.fastForward pointsAllowed model.initialSeed)
            in
            ( { model
                | pointsAllowed = pointsAllowed
                , placements = updatedPlacements
              }
            , sendPlacements updatedPlacements
            )

        SelectMode mode ->
            case mode of
                ForcingMoves ->
                    ( { model | mode = mode, alerts = List.concat [ [ "Coming soon! Sorry for the click bait." ], model.alerts ] }, Cmd.none )

                _ ->
                    ( { model | mode = mode, alerts = [] }, Cmd.none )

        SubmitLesson ->
            ( { model | submitting = True }, postLessonCmd model )

        RegenerateSeed currentTime ->
            let
                nextSeed =
                    findNextSeed currentTime

                updatedPlacements =
                    generate [] model.pointsAllowed (Random.fastForward model.pointsAllowed nextSeed)
            in
            ( { model | initialSeed = nextSeed, placements = updatedPlacements }, sendPlacements updatedPlacements )

        GetSeed ->
            ( model, Task.perform RegenerateSeed Time.now )

        FetchSeedCompleted result ->
            fetchSeedCompleted model result

        PostLessonCompleted result ->
            postLessonCompleted model result

        Record ->
            ( handleGameUpdate { model | initialGameState = Just model.currentGameState } model.currentGameState, Cmd.none )

        ChessMsg chessMsg ->
            let
                config =
                    { toMsg = ChessMsg
                    , onFenChanged = HandleGameUpdate
                    }

                ( updatedChessModel, chessCmd ) =
                    Chess.update config chessMsg model.chessModel
            in
            ( { model | chessModel = updatedChessModel }, chessCmd )

        WriteHint position content ->
            case model.initialGameState of
                Just _ ->
                    ( { model | store = saveHint model.store (findStorageKey model.currentGameState) position content }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        WriteDefaultMessage content ->
            case model.initialGameState of
                Just _ ->
                    ( { model | store = saveDefaultMessage model.store (findStorageKey model.currentGameState) content }, Cmd.none )

                _ ->
                    ( model, Cmd.none )



---- FETCH SEED ----


api : String -> String
api apiEndpoint =
    apiEndpoint ++ "/"


findNextSeed : Time.Time -> Random.Seed
findNextSeed fdsa =
    Random.initialSeed <| Basics.floor <| Time.inMilliseconds fdsa


fetchSeedCompleted : Model -> Result Http.Error Int -> ( Model, Cmd Msg )
fetchSeedCompleted model result =
    case result of
        Ok seed ->
            let
                nextSeed =
                    Random.initialSeed seed

                updatedPlacements =
                    generate [] model.pointsAllowed (Random.fastForward model.pointsAllowed nextSeed)
            in
            ( { model | initialSeed = nextSeed, placements = updatedPlacements }, sendPlacements updatedPlacements )

        Err _ ->
            ( model, Cmd.none )


postLessonCmd : Model -> Cmd Msg
postLessonCmd { initialGameState, store, apiEndpoint } =
    case initialGameState of
        Nothing ->
            Debug.crash "NO GAME STATE, DISABLE THIS BUTTON UNTIL YOU HAVE ONE"

        Just gameState ->
            let
                body =
                    jsonBody
                        (E.object
                            [ ( "title", E.string "The net" )
                            , ( "state", E.string gameState )
                            , ( "store", encodeStore store )
                            ]
                        )

                request =
                    Http.post (api apiEndpoint ++ "lessons") body (D.succeed "cake")
            in
            Http.send PostLessonCompleted request


encodeStore : Store -> E.Value
encodeStore (Store store) =
    E.object <| List.map (\( f, a ) -> ( f, encodeFrame a )) <| Dict.toList store


encodeFrame : Frame -> E.Value
encodeFrame { squares, defaultMessage } =
    let
        (Hints hints) =
            squares

        squaresEncoder =
            List.map (\( f, a ) -> ( f, E.string a )) <| Dict.toList hints
    in
    E.object
        [ ( "squares", E.object squaresEncoder )
        , ( "defaultMessage", E.string defaultMessage )
        ]


postLessonCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
postLessonCompleted model result =
    case result of
        Ok thing ->
            ( { model | submitting = False }, Cmd.none )

        Err _ ->
            ( { model | submitting = False }, Cmd.none )



---- GAME UPDATE ----


handleGameUpdate : Model -> String -> Model
handleGameUpdate model newFen =
    let
        updatedState =
            case Chess.fromFen newFen of
                Nothing ->
                    Debug.crash "BAD FEN"

                Just s ->
                    s

        blankFrame =
            Frame (Hints Dict.empty) "Click the yellow square"

        storageKey =
            findStorageKey newFen

        (Store rawStore) =
            model.store

        maybeFrame =
            Dict.get storageKey rawStore

        updatedStore =
            case ( maybeFrame, model.initialGameState ) of
                ( Nothing, Just _ ) ->
                    Store (Dict.insert storageKey blankFrame rawStore)

                ( _, _ ) ->
                    model.store
    in
    { model
        | chessModel = updatedState
        , store = updatedStore
        , currentGameState = newFen
    }


saveHint : Store -> String -> String -> String -> Store
saveHint (Store store) frameKey positionKey content =
    let
        maybeFrame =
            Dict.get frameKey store

        frame =
            case maybeFrame of
                Nothing ->
                    Debug.crash "impossible state is not impossible :("

                Just f ->
                    f

        (Hints rawHints) =
            frame.squares

        maybeSquareHint =
            Dict.get positionKey rawHints

        updatedSquareHints =
            case maybeSquareHint of
                Nothing ->
                    Dict.insert positionKey content rawHints

                Just hint ->
                    Dict.update positionKey
                        (\_ -> Just content)
                        rawHints

        updatedFrame =
            { frame | squares = Hints updatedSquareHints }
    in
    Store (Dict.update frameKey (\_ -> Just updatedFrame) store)


saveDefaultMessage : Store -> String -> String -> Store
saveDefaultMessage (Store store) frameKey content =
    let
        maybeFrame =
            Dict.get frameKey store

        frame =
            case maybeFrame of
                Nothing ->
                    Debug.crash "impossible state is not impossible :("

                Just f ->
                    f

        updatedFrame =
            { frame | defaultMessage = content }
    in
    Store (Dict.update frameKey (\_ -> Just updatedFrame) store)


sendPlacements : List Placement -> Cmd msg
sendPlacements placements =
    BuilderJs.fromElm
        (E.object
            [ ( "tag", E.string "SEND_PLACEMENTS" )
            , ( "placements", pplacements placements )
            ]
        )


pplacements : List Placement -> E.Value
pplacements placements =
    E.list (List.map (\p -> placement p) placements)


placement : Placement -> E.Value
placement placement =
    E.object
        [ ( "square", square placement.square )
        , ( "piece", piece placement.piece )
        , ( "team", team placement.team )
        ]


square : SquareLocation -> E.Value
square square =
    let
        row =
            case square.x of
                1 ->
                    "a"

                2 ->
                    "b"

                3 ->
                    "c"

                4 ->
                    "d"

                5 ->
                    "e"

                6 ->
                    "f"

                7 ->
                    "g"

                8 ->
                    "h"

                _ ->
                    Debug.crash "not valid x parameter :("
    in
    E.string (row ++ toString square.y)


piece : Piece -> E.Value
piece piece =
    case piece of
        Monarch ->
            E.string "k"

        Hand ->
            E.string "q"

        Rook ->
            E.string "r"

        Bishop ->
            E.string "b"

        Knight ->
            E.string "n"

        Pawn ->
            E.string "p"


team : Player -> E.Value
team team =
    case team of
        Black ->
            E.string "b"

        White ->
            E.string "w"


generate : List Placement -> Int -> Random.Seed -> List Placement
generate placements pointsAllowed currentSeed =
    let
        hasBothMonarchs =
            2 == List.length (List.filter (is Monarch) placements)

        atMaxScore =
            currentTotal placements == pointsAllowed
    in
    if hasBothMonarchs && atMaxScore then
        placements

    else
        let
            ( updatedPlacements, nextSeed ) =
                generatePlacement placements pointsAllowed currentSeed
        in
        generate updatedPlacements pointsAllowed nextSeed


generatePlacement : List Placement -> Int -> Random.Seed -> ( List Placement, Random.Seed )
generatePlacement placements pointsAllowed currentSeed =
    let
        ( selectedSquare, updatedSeed ) =
            Random.step squareGenerator currentSeed

        ( piece, moarUpdatedSeed ) =
            Random.step pieceGenerator updatedSeed

        ( team, evenMoarUpdatedSeed ) =
            Random.step teamGenerator moarUpdatedSeed

        constructed =
            { square = selectedSquare
            , piece = piece
            , team = team
            }
    in
    ( conditionalUpdatedBoard pointsAllowed placements constructed, evenMoarUpdatedSeed )


conditionalUpdatedBoard : Int -> List Placement -> Placement -> List Placement
conditionalUpdatedBoard pointsAllowed placements constructed =
    if isLegal pointsAllowed placements constructed then
        placements ++ [ constructed ]

    else
        placements



-- VALIDATION


type alias State =
    { placements : List Placement
    , constructed : Placement
    }


type Validation
    = Valid
    | Invalid


isLegal : Int -> List Placement -> Placement -> Bool
isLegal pointsAllowed placements constructed =
    let
        state =
            State placements constructed
    in
    List.all (\validate -> validate state == Valid)
        [ squareNotOpen
        , monarchAlreadyPlaced
        , monarchsNotAdjacent
        , notTooManyPawns
        , pawnsNotInEndRows
        , doesntLeadToBalancedGame
        , alreadyPlacedMaximum
        , notEnoughPointsRemaining pointsAllowed
        ]


squareNotOpen : State -> Validation
squareNotOpen { placements, constructed } =
    if List.any (\placement -> placement.square == constructed.square) placements then
        Invalid

    else
        Valid


monarchAlreadyPlaced : State -> Validation
monarchAlreadyPlaced { placements, constructed } =
    if List.any (\placement -> placement.piece == Monarch && constructed.piece == Monarch && placement.team == constructed.team) placements then
        Invalid

    else
        Valid


monarchsNotAdjacent : State -> Validation
monarchsNotAdjacent { placements, constructed } =
    let
        findMonarch team =
            List.filter
                (\p -> p.team == team && p.piece == Monarch)
                (constructed :: placements)
    in
    {- (-2,-1)
       (-1,-1) . (-1,0) . (-1,1)
       ( 0,-1) . ( 0,0) . ( 0,1) . ( 0,5)
       ( 1,-1) . ( 1,0) . ( 1,1)
    -}
    case ( findMonarch Black, findMonarch White ) of
        ( [ player ], [ opponent ] ) ->
            if
                (abs (player.square.x - opponent.square.x) <= 1)
                    && (abs (player.square.y - opponent.square.y) <= 1)
            then
                Invalid

            else
                Valid

        _ ->
            Valid


notTooManyPawns : State -> Validation
notTooManyPawns { placements, constructed } =
    let
        forConstructed =
            List.filter (is Pawn) (constructed :: placements)
                |> List.filter ((==) constructed.team << .team)
    in
    if List.length forConstructed > 8 then
        Invalid

    else
        Valid


pawnsNotInEndRows : State -> Validation
pawnsNotInEndRows { constructed } =
    if
        (constructed.piece == Pawn)
            && (constructed.square.y == 1 || constructed.square.y == 8)
    then
        Invalid

    else
        Valid


doesntLeadToBalancedGame : State -> Validation
doesntLeadToBalancedGame { placements, constructed } =
    let
        ( constructedPlacements, otherPlacements ) =
            List.partition ((==) constructed.team << .team) placements
    in
    if
        (findPointValueFromPiece constructed.piece > 0)
            && (currentTotal constructedPlacements > currentTotal otherPlacements)
    then
        Invalid

    else
        Valid


alreadyPlacedMaximum : State -> Validation
alreadyPlacedMaximum { placements, constructed } =
    let
        applicablePlacements =
            List.filter (\placement -> placement.piece == constructed.piece && placement.team == constructed.team) placements

        maxValue =
            maximumPieceCount (List.head applicablePlacements)
    in
    if maxValue == List.length applicablePlacements then
        Invalid

    else
        Valid


maximumPieceCount : Maybe Placement -> Int
maximumPieceCount maybePlacement =
    case maybePlacement of
        Nothing ->
            -1

        Just placement ->
            case placement.piece of
                Monarch ->
                    1

                Hand ->
                    1

                Rook ->
                    2

                Bishop ->
                    2

                Knight ->
                    2

                Pawn ->
                    8


notEnoughPointsRemaining : Int -> State -> Validation
notEnoughPointsRemaining pointsAllowed { placements, constructed } =
    if pointsAllowed < currentTotal placements + findPointValueFromPiece constructed.piece then
        Invalid

    else
        Valid


is : Piece -> Placement -> Bool
is piece placement =
    placement.piece == piece


currentTotal : List Placement -> Int
currentTotal placements =
    List.foldr (\placement total -> total + findPointValueFromPiece placement.piece) 0 placements


findPointValueFromPiece : Piece -> Int
findPointValueFromPiece piece =
    case piece of
        Monarch ->
            0

        Hand ->
            9

        Rook ->
            5

        Bishop ->
            3

        Knight ->
            3

        Pawn ->
            1


squareGenerator : Random.Generator SquareLocation
squareGenerator =
    Random.map2 SquareLocation (Random.int 1 8) (Random.int 1 8)


pieceGenerator : Random.Generator Piece
pieceGenerator =
    Random.choices <|
        List.map Random.constant
            [ Monarch
            , Hand
            , Rook
            , Bishop
            , Knight
            , Pawn
            ]


teamGenerator : Random.Generator Player
teamGenerator =
    Random.choice Black White



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ viewNavbar
        , viewAlerts model
        , section [ H.class "section" ]
            [ div [ H.class "container" ]
                [ div [ H.class "columns is-centered is-variable is-8" ]
                    [ div
                        [ H.class "column is-narrow"
                        ]
                        [ Html.map ChessMsg (Chess.view findConfig model.chessModel)
                        , section [ H.class "columns" ]
                            [ div [ H.class "column is-two-thirds" ] []
                            , div [ H.class "column is-one-third" ]
                                [ h2 [ H.class "subtitle is-3" ] [ text <| "Level " ++ toString model.pointsAllowed ]
                                ]
                            ]
                        ]
                    , div [ H.class "column is-narrow has-text-centered" ]
                        [ h3 [ H.class "subtitle is-5" ] [ text "Mode" ]
                        , viewModeSelection model
                        , viewHints model
                        , h3 [ H.class "subtitle is-5" ] [ text "Current Level" ]
                        , makeSlider model
                        , viewActionMenu model
                        ]
                    ]
                ]
            ]
        ]


viewNavbar : Html Msg
viewNavbar =
    nav
        [ H.class "navbar has-shadow is-spaced" ]
        [ div [ H.class "container" ]
            [ div [ H.class "navbar-brand" ]
                [ a
                    [ H.class "navbar-item"
                    , H.href "https://github.com/7hoenix/procedural-gen"
                    , H.target "_blank"
                    ]
                    [ h1 [ H.class "title" ] [ text "Builder" ]
                    ]
                , div
                    [ H.class "navbar-burger, burger"
                    ]
                    [ span
                        [ H.attribute "aria-hidden" "true"
                        ]
                        []
                    , span
                        [ H.attribute "aria-hidden" "true"
                        ]
                        []
                    , span
                        [ H.attribute "aria-hidden" "true"
                        ]
                        []
                    ]
                ]
            , div [ H.class "navbar-menu" ]
                [ div [ H.class "navbar-start" ]
                    [ a [ H.class "button", href "http://beta.chesstrained.com/#/simulation" ] [ text "Try it out" ]
                    ]
                , div [ H.class "navbar-end" ] []
                ]
            ]
        ]



---- ALERTS ----


viewAlerts : Model -> Html Msg
viewAlerts model =
    case List.head model.alerts of
        Just alert ->
            section [ H.class "section" ]
                [ div
                    [ H.class "columns is-centered is-variable is-8"
                    ]
                    [ text alert ]
                ]

        _ ->
            div [] []



---- MODESELECTION ----


viewModeSelection : Model -> Html Msg
viewModeSelection model =
    let
        isChecked =
            \x -> model.mode == x
    in
    div [ H.class "box", H.class "control" ]
        [ radio (SelectMode Basic) " Basic" "mode" (isChecked Basic)
        , radio (SelectMode ForcingMoves) " Forcing Moves" "mode" (isChecked ForcingMoves)
        ]


radio : msg -> String -> String -> Bool -> Html msg
radio msg buttonText name isChecked =
    label [ H.class "radio" ]
        [ input [ type_ "radio", H.name name, onClick msg, H.checked isChecked ] []
        , text buttonText
        ]


makeSlider : Model -> Html Msg
makeSlider model =
    div []
        [ text "1  "
        , input
            [ type_ "range"
            , H.min "1"
            , H.max "10"
            , defaultValue <| toString model.pointsAllowed
            , on "input" (targetValue |> D.andThen parseInt)
            ]
            []
        , text "  10"
        ]


parseInt : String -> D.Decoder Msg
parseInt rawString =
    case String.toInt rawString of
        Err err ->
            D.fail err

        Ok int ->
            D.succeed (HandleSliderChange int)



---- HINTS ----


viewHints : Model -> Html Msg
viewHints model =
    let
        maybeFrame =
            getFrame model.store model.currentGameState
    in
    case maybeFrame of
        Nothing ->
            text ""

        Just frame ->
            div []
                [ h3 [ H.class "subtitle is-5" ] [ text "Square Hints" ]
                , div [] [ input [ H.placeholder "The enemies gate is down", H.value frame.defaultMessage, Html.Events.onInput WriteDefaultMessage ] [] ]
                , viewSquareHints (getSquaresSelected model.chessModel) frame.squares
                ]


getFrameHints : Store -> String -> Maybe Hints
getFrameHints store gameState =
    Maybe.map (\frame -> frame.squares) <| getFrame store gameState


getFrame : Store -> String -> Maybe Frame
getFrame (Store store) gameState =
    Dict.get (findStorageKey gameState) store



---- LESSON SYSTEM ----
-- TODO: see if we can't centralize these types in a place where both projects can use them...


type alias Lesson =
    { store : Store
    , title : String
    , initialGameState : String
    }


type alias PartialFenState =
    String


type Store
    = Store (Dict PartialFenState Frame)


type alias Frame =
    { squares : Hints
    , defaultMessage : String
    }


type alias SquareKey =
    String


type Hints
    = Hints (Dict SquareKey String)


viewSquareHints : List Position -> Hints -> Html Msg
viewSquareHints selectedSquares (Hints hints) =
    case selectedSquares of
        [] ->
            text "Click any square"

        _ ->
            div []
                (List.map
                    (\square ->
                        let
                            storageKey =
                                toCommand square

                            hint =
                                case Dict.get storageKey hints of
                                    Nothing ->
                                        ""

                                    Just h ->
                                        h
                        in
                        div []
                            [ input [ H.placeholder storageKey, H.value hint, Html.Events.onInput (WriteHint storageKey) ] []
                            ]
                    )
                    selectedSquares
                )


findStorageKey : String -> String
findStorageKey fullFenState =
    String.join " " <| List.take 2 <| String.words fullFenState


toCommand : Position -> String
toCommand position =
    let
        -- TODO: move this function into elm-chess/Chess.Data.Position
        ( row, column ) =
            toRowColumn position
    in
    String.fromChar (Char.fromCode <| column + 97) ++ toString (8 - row)



---- ACTIONMENU ----


viewActionMenu : Model -> Html Msg
viewActionMenu model =
    div []
        [ div [ H.class "level" ]
            [ div [ H.class "level-item" ] [ button [ onClick GetSeed, H.class "button is-primary" ] [ text "Regenerate" ] ]
            , case model.initialGameState of
                Nothing ->
                    div [ H.class "level-item" ] [ button [ onClick Record, H.class "button is-info" ] [ text "Start Recording" ] ]

                Just _ ->
                    div [ H.class "level-item" ]
                        [ button (loadingButtonAttributes (onClick SubmitLesson) "is-info" model.submitting)
                            [ text "Submit Lesson" ]
                        ]
            ]
        ]


loadingButtonAttributes clickHandler color isLoading =
    let
        displayLoading =
            if isLoading then
                "is-loading"

            else
                ""
    in
    [ clickHandler
    , H.class "button"
    , H.class color
    , H.class displayLoading
    ]



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ BuilderJs.fromJs decodeFen
        , Sub.map ChessMsg (Chess.subscriptions model.chessModel)
        ]


decodeFen : E.Value -> Msg
decodeFen value =
    let
        result =
            D.decodeValue (D.field "position" D.string) value
    in
    case result of
        Ok fen ->
            HandleGameUpdate fen

        Err error ->
            Debug.crash ("fen decoding failed: " ++ error)



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }

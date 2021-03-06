module Main exposing (main)

import Api
import Browser
import BuilderJs
import Char
import Chess exposing (Msg, State, ValidatedFen, blankState, blankValidatedFen, fromValidatedFen, getBoard, getRaw, getSquaresSelected, getTeam, setTeam, subscriptions, update, view)
import Chess.Data.Player exposing (Player(..))
import Chess.Data.Position exposing (Position, toRowColumn)
import Chess.View.Board
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, fieldset, h1, h2, h3, h4, h5, input, label, nav, p, section, span, text, textarea)
import Html.Attributes as H exposing (href, max, min, target, type_)
import Html.Events exposing (on, onClick, targetValue)
import Http exposing (Request, jsonBody)
import Json.Decode as D
import Json.Encode as E
import Random
import Task
import Time



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.document
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }



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



---- DUPLICTED TYPE ATM ----


type FeatureSet
    = Full
    | NoMoves
    | PlayerMoves
    | OpponentMoves


type alias Model =
    { alert : Maybe String
    , apiEndpoint : String
    , baseEngineUrl : String
    , mode : SupportedMode
    , store : Store
    , currentGameState : ValidatedFen
    , featureSet : FeatureSet
    , initialGameState : Maybe ValidatedFen
    , initialSeed : Random.Seed
    , placements : List Placement
    , pointsAllowed : Int
    , startingTeam : Player
    , submitting : Bool
    , chessModel : Chess.State
    }


type alias Flags =
    { apiEndpoint : String
    , baseEngineUrl : String
    , initialSeed : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    blankState (Random.initialSeed flags.initialSeed) 1 flags.apiEndpoint flags.baseEngineUrl


blankState : Random.Seed -> Int -> String -> String -> ( Model, Cmd Msg )
blankState initialSeed pointsAllowed apiEndpoint baseEngineUrl =
    let
        initialPlacements =
            generate [] pointsAllowed initialSeed

        blankValidatedFen =
            Chess.blankValidatedFen
    in
    ( { apiEndpoint = apiEndpoint
      , featureSet = Full
      , baseEngineUrl = baseEngineUrl
      , mode = Basic
      , store = Store Dict.empty
      , currentGameState = blankValidatedFen
      , initialGameState = Nothing
      , initialSeed = initialSeed
      , placements = initialPlacements
      , pointsAllowed = 1
      , submitting = False
      , startingTeam = Black
      , alert = Nothing
      , chessModel = Chess.fromValidatedFen blankValidatedFen
      }
    , sendPlacements initialPlacements
    )



-- TODO: SUPPORT MOBILE LATER


findConfig : Chess.View.Board.Config
findConfig =
    { each = "5em", between = "0.15em", borderSize = "0.3em" }



---- UPDATE ----


type Msg
    = ChessMsg Chess.Msg
    | ClearAlert
    | Error String
    | FindBestMove
    | GetSeed
    | HandleGameUpdate ValidatedFen
    | HandleSliderChange Int
    | PostLessonCompleted (Result Http.Error String)
    | Record
    | RegenerateSeed Time.Posix
    | Reset
    | SelectFeatureSet FeatureSet
    | SelectMode SupportedMode
    | SelectTeam Player
    | SubmitLesson
    | Validate
    | WriteDefaultMessage String
    | WriteHint String String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChessMsg chessMsg ->
            handleChessMsg model chessMsg

        ClearAlert ->
            ( { model | alert = Nothing }, Cmd.none )

        Error err ->
            ( { model | alert = Just err }, Cmd.none )

        FindBestMove ->
            findBestMove model

        GetSeed ->
            ( model, Task.perform RegenerateSeed Time.now )

        HandleGameUpdate fen ->
            ( handleGameUpdate model fen, Cmd.none )

        HandleSliderChange pointsAllowed ->
            handleSliderChange pointsAllowed model

        PostLessonCompleted result ->
            postLessonCompleted model result

        Record ->
            startRecording model

        RegenerateSeed currentTime ->
            regenerateSeed currentTime model

        Reset ->
            blankState model.initialSeed model.pointsAllowed model.apiEndpoint model.baseEngineUrl

        SelectFeatureSet featureSet ->
            ( selectFeatureSet featureSet model, Cmd.none )

        SelectMode mode ->
            ( selectMode mode model, Cmd.none )

        SelectTeam team ->
            ( { model | startingTeam = team }, Cmd.none )

        SubmitLesson ->
            postLessonCmd { model | submitting = True }

        Validate ->
            ( model, sendPlacements model.placements )

        WriteDefaultMessage content ->
            writeDefaultMessage content model

        WriteHint position content ->
            writeHint position content model


handleChessMsg : Model -> Chess.Msg -> ( Model, Cmd Msg )
handleChessMsg model chessMsg =
    let
        config =
            { toMsg = ChessMsg
            , onFenChanged = HandleGameUpdate
            , isRecording = isRecording model.initialGameState
            }

        ( updatedChessModel, chessCmd ) =
            Chess.update config chessMsg model.chessModel
    in
    ( { model | chessModel = updatedChessModel }, chessCmd )



---- FETCH SEED ----


fetchSeedCompleted : Model -> Result Http.Error Int -> ( Model, Cmd Msg )
fetchSeedCompleted model result =
    case result of
        Ok seed ->
            let
                nextSeed =
                    Random.initialSeed seed

                updatedPlacements =
                    generate [] model.pointsAllowed (fastForward model.pointsAllowed nextSeed)
            in
            ( { model | initialSeed = nextSeed, placements = updatedPlacements }, sendPlacements updatedPlacements )

        Err _ ->
            ( model, Cmd.none )


postLessonCmd : Model -> ( Model, Cmd Msg )
postLessonCmd ({ initialGameState, store, apiEndpoint } as model) =
    case initialGameState of
        Nothing ->
            ( { model | alert = Just "NO GAME STATE" }, Cmd.none )

        Just validatedFen ->
            let
                body =
                    jsonBody
                        (E.object
                            [ ( "title", E.string "The net" )
                            , ( "state", E.string (getRaw validatedFen) )
                            , ( "store", encodeStore store )
                            ]
                        )

                request =
                    Http.post (api apiEndpoint ++ "lessons") body (D.succeed "cake")
            in
            ( model, Http.send PostLessonCompleted request )


encodeStore : Store -> E.Value
encodeStore (Store store) =
    E.object <| List.map (\( f, a ) -> ( f, encodeFrame a )) <| Dict.toList store


encodeFrame : Frame -> E.Value
encodeFrame { featureSet, squares, defaultMessage } =
    let
        (Hints hints) =
            squares

        squaresEncoder =
            List.map (\( f, a ) -> ( f, E.string a )) <| Dict.toList hints
    in
    E.object
        [ ( "featureSet", encodeFeatureSet featureSet )
        , ( "squares", E.object squaresEncoder )
        , ( "defaultMessage", E.string defaultMessage )
        ]


encodeFeatureSet : FeatureSet -> E.Value
encodeFeatureSet featureSet =
    case featureSet of
        Full ->
            E.string "Full"

        NoMoves ->
            E.string "NoMoves"

        PlayerMoves ->
            E.string "PlayerMoves"

        OpponentMoves ->
            E.string "OpponentMoves"


postLessonCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
postLessonCompleted model result =
    case result of
        Ok thing ->
            ( { model | submitting = False }, Cmd.none )

        Err _ ->
            ( { model | submitting = False }, Cmd.none )


startRecording : Model -> ( Model, Cmd Msg )
startRecording model =
    let
        initialGameState =
            Chess.setTeam model.startingTeam model.currentGameState
    in
    ( handleGameUpdate { model | initialGameState = Just initialGameState } initialGameState, Cmd.none )


regenerateSeed : Time.Posix -> Model -> ( Model, Cmd Msg )
regenerateSeed currentTime model =
    let
        nextSeed =
            findNextSeed currentTime

        updatedPlacements =
            generate [] model.pointsAllowed (fastForward model.pointsAllowed nextSeed)
    in
    ( { model | initialSeed = nextSeed, placements = updatedPlacements }, sendPlacements updatedPlacements )


selectFeatureSet : FeatureSet -> Model -> Model
selectFeatureSet featureSet model =
    case ( model.initialGameState, findFrame model.store (findStorageKey model.currentGameState) ) of
        ( Just _, Just frame ) ->
            { model | store = saveFeatureSet model.store (findStorageKey model.currentGameState) frame featureSet }

        ( _, _ ) ->
            { model | featureSet = featureSet }


selectMode : SupportedMode -> Model -> Model
selectMode mode model =
    case mode of
        Basic ->
            { model | mode = mode, alert = Nothing }

        ForcingMoves ->
            { model | mode = mode, alert = Just "Coming soon!" }


writeDefaultMessage : String -> Model -> ( Model, Cmd Msg )
writeDefaultMessage content model =
    case ( model.initialGameState, findFrame model.store (findStorageKey model.currentGameState) ) of
        ( Just _, Just frame ) ->
            ( { model | store = saveDefaultMessage model.store (findStorageKey model.currentGameState) frame content }, Cmd.none )

        ( Just _, Nothing ) ->
            ( { model | alert = Just "You must have a frame in order to write to it." }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


writeHint : String -> String -> Model -> ( Model, Cmd Msg )
writeHint position content model =
    case ( model.initialGameState, findFrame model.store (findStorageKey model.currentGameState) ) of
        ( Just _, Just frame ) ->
            ( { model | store = saveHint model.store (findStorageKey model.currentGameState) frame position content }, Cmd.none )

        ( Just _, Nothing ) ->
            ( { model | alert = Just "You must have a frame in order to write a hint." }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


findFrame : Store -> String -> Maybe Frame
findFrame (Store store) frameKey =
    Dict.get frameKey store


findBestMove : Model -> ( Model, Cmd Msg )
findBestMove model =
    ( model
    , Api.best model.baseEngineUrl model.currentGameState
        |> Task.attempt
            (either
                (\err -> Error "Best move api failure")
                (\fen -> HandleGameUpdate fen)
            )
    )



---- GAME UPDATE ----


handleGameUpdate : Model -> ValidatedFen -> Model
handleGameUpdate model validatedFen =
    handleGameUpdateHelp model validatedFen <| Chess.fromValidatedFen validatedFen


handleGameUpdateHelp : Model -> ValidatedFen -> Chess.State -> Model
handleGameUpdateHelp model newFen updatedState =
    let
        blankFrame =
            Frame model.featureSet (Hints Dict.empty) ""

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


handleSliderChange : Int -> Model -> ( Model, Cmd Msg )
handleSliderChange pointsAllowed model =
    let
        updatedPlacements =
            generate [] pointsAllowed (fastForward pointsAllowed model.initialSeed)
    in
    ( { model
        | pointsAllowed = pointsAllowed
        , placements = updatedPlacements
      }
    , sendPlacements updatedPlacements
    )


saveHint : Store -> String -> Frame -> String -> String -> Store
saveHint (Store store) frameKey frame positionKey content =
    let
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


saveDefaultMessage : Store -> String -> Frame -> String -> Store
saveDefaultMessage (Store store) frameKey frame content =
    let
        updatedFrame =
            { frame | defaultMessage = content }
    in
    Store (Dict.update frameKey (\_ -> Just updatedFrame) store)


saveFeatureSet : Store -> String -> Frame -> FeatureSet -> Store
saveFeatureSet (Store store) frameKey frame featureSet =
    let
        updatedFrame =
            { frame | featureSet = featureSet }
    in
    Store (Dict.update frameKey (\_ -> Just updatedFrame) store)


sendPlacements : List Placement -> Cmd msg
sendPlacements placements =
    BuilderJs.fromElm
        (E.object
            [ ( "tag", E.string "SEND_PLACEMENTS" )
            , ( "placements", placementsEncoder placements )
            ]
        )


placementsEncoder : List Placement -> E.Value
placementsEncoder placements =
    E.list placementEncoder placements


placementEncoder : Placement -> E.Value
placementEncoder placement =
    E.object
        [ ( "square", squareEncoder placement.square )
        , ( "piece", pieceEncoder placement.piece )
        , ( "team", teamEncoder placement.team )
        ]


squareEncoder : SquareLocation -> E.Value
squareEncoder square =
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
                    "not valid x parameter :("
    in
    E.string (row ++ String.fromInt square.y)


pieceEncoder : Piece -> E.Value
pieceEncoder piece =
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


teamEncoder : Player -> E.Value
teamEncoder team =
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
    List.all (\validate -> validate (State placements constructed) == Valid)
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
    Random.uniform Monarch
        [ Hand
        , Rook
        , Bishop
        , Knight
        , Pawn
        ]


teamGenerator : Random.Generator Player
teamGenerator =
    Random.uniform Black [ White ]



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Builder"
    , body = viewApp model
    }


viewApp : Model -> List (Html Msg)
viewApp model =
    [ viewNavbar model
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
                            [ h2 [ H.class "subtitle is-3" ] [ text <| "Level " ++ String.fromInt model.pointsAllowed ]
                            ]
                        ]
                    ]
                , div [ H.class "column is-narrow has-text-centered" ]
                    [ viewSidebar model
                    ]
                ]
            ]
        ]
    ]


viewNavbar : Model -> Html Msg
viewNavbar model =
    nav
        [ H.class "navbar has-shadow is-spaced" ]
        [ div [ H.class "container" ]
            [ div [ H.class "navbar-brand" ]
                [ a
                    [ H.class "navbar-item"
                    , H.href "https://github.com/7hoenix/builder"
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
                , div [ H.class "navbar-end" ]
                    [ case model.initialGameState of
                        Nothing ->
                            p [] [ text "" ]

                        Just _ ->
                            p [ H.class "title is-5" ] [ text ("Level " ++ String.fromInt model.pointsAllowed ++ ", Basic") ]
                    ]
                ]
            ]
        ]



---- ALERTS ----


viewAlerts : Model -> Html Msg
viewAlerts model =
    case model.alert of
        Just alert ->
            div
                [ H.class "notification"
                ]
                [ button [ H.class "delete", onClick ClearAlert ] []
                , text alert
                ]

        Nothing ->
            div [] []



---- SIDEBAR ----


viewSidebar : Model -> Html Msg
viewSidebar model =
    div []
        [ viewMode model
        , viewFeatureSet model
        , viewTeam model
        , viewSlider model
        , viewLesson model
        , viewActionMenu model
        ]



---- MODE ----


viewMode : Model -> Html Msg
viewMode model =
    let
        isSelected : SupportedMode -> Bool
        isSelected =
            \x -> model.mode == x
    in
    case model.initialGameState of
        Nothing ->
            div [ H.class "box", H.class "control" ]
                [ h3 [ H.class "subtitle is-5" ] [ text "Mode" ]
                , radio (SelectMode Basic) " Basic" "mode" (isSelected Basic)
                ]

        Just _ ->
            text ""


viewFeatureSet : Model -> Html Msg
viewFeatureSet model =
    let
        isSelected : FeatureSet -> Bool
        isSelected =
            \x -> model.featureSet == x
    in
    case model.initialGameState of
        Nothing ->
            text ""

        Just _ ->
            div [ H.class "box", H.class "control" ]
                [ h3 [ H.class "subtitle is-5" ] [ text "Feature Set" ]
                , radio (SelectFeatureSet Full) " Full" "featureSet" (isSelected Full)
                , radio (SelectFeatureSet NoMoves) " NoMoves" "featureSet" (isSelected NoMoves)
                , radio (SelectFeatureSet PlayerMoves) " PlayerMoves" "featureSet" (isSelected PlayerMoves)
                , radio (SelectFeatureSet OpponentMoves) " OpponentMoves" "featureSet" (isSelected OpponentMoves)
                ]


viewTeam : Model -> Html Msg
viewTeam model =
    let
        isSelected : Player -> Bool
        isSelected team =
            Chess.getTeam model.currentGameState == team
    in
    case model.initialGameState of
        Nothing ->
            div [ H.class "box", H.class "control" ]
                [ h3 [ H.class "subtitle is-5" ] [ text "Turn" ]
                , radio (SelectTeam White) " White" "team" (isSelected White)
                , radio (SelectTeam Black) " Black" "team" (isSelected Black)
                ]

        Just _ ->
            text ""


radio : msg -> String -> String -> Bool -> Html msg
radio msg buttonText name isSelected =
    label [ H.class "radio" ]
        [ input [ type_ "radio", H.name name, onClick msg, H.selected isSelected ] []
        , text buttonText
        ]


parseInt : String -> D.Decoder Msg
parseInt rawString =
    case String.toInt rawString of
        Nothing ->
            D.fail <| rawString ++ " is not parseable to an integer"

        Just int ->
            D.succeed (HandleSliderChange int)



---- HINTS ----


viewLesson : Model -> Html Msg
viewLesson model =
    let
        maybeFrame =
            getFrame model.store model.currentGameState

        defaultLessonMessage =
            "## This is the default frame content.\n\n" ++ "It supports **markdown**."

        teamToString team =
            case team of
                White ->
                    "white"

                Black ->
                    "black"
    in
    case maybeFrame of
        Nothing ->
            text ""

        Just frame ->
            div [ H.class "box", H.class "control" ]
                [ div [] [ h5 [ H.class "title is-6" ] [ text "Current Lesson" ] ]
                , div [] [ text <| "Turn: " ++ (teamToString <| Chess.getTeam model.currentGameState) ]
                , div []
                    [ textarea
                        [ H.class "textarea"
                        , H.placeholder defaultLessonMessage
                        , H.value frame.defaultMessage
                        , Html.Events.onInput WriteDefaultMessage
                        ]
                        []
                    ]
                , viewSquareHints (getSquaresSelected model.chessModel) frame.squares
                , div [] [ button [ onClick FindBestMove, H.class "button is-small is-success" ] [ text "Find Best Move" ] ]
                ]


getFrame : Store -> ValidatedFen -> Maybe Frame
getFrame (Store store) gameState =
    Dict.get (findStorageKey gameState) store


viewSlider : Model -> Html Msg
viewSlider model =
    case model.initialGameState of
        Just _ ->
            text ""

        Nothing ->
            div
                [ H.class "box", H.class "control" ]
                [ h3 [ H.class "subtitle is-5" ] [ text "Current Level" ]
                , makeSlider model
                ]


makeSlider : Model -> Html Msg
makeSlider model =
    case model.initialGameState of
        Nothing ->
            div []
                [ text "1  "
                , input
                    [ type_ "range"
                    , H.min "1"
                    , H.max "10"
                    , H.value <| String.fromInt model.pointsAllowed
                    , on "input" (targetValue |> D.andThen parseInt)
                    ]
                    []
                , text "  10"
                ]

        Just _ ->
            div []
                [ text <|
                    String.fromInt model.pointsAllowed
                        ++ " Points of Material Allowed"
                ]



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
    { featureSet : FeatureSet
    , squares : Hints
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
            text "Click any square to add helper text"

        _ ->
            div []
                (List.map
                    (\selectedSquare ->
                        let
                            storageKey =
                                toCommand selectedSquare

                            hint =
                                case Dict.get storageKey hints of
                                    Nothing ->
                                        ""

                                    Just h ->
                                        h
                        in
                        div []
                            [ textarea [ H.class "textarea", H.placeholder storageKey, H.value hint, Html.Events.onInput (WriteHint storageKey) ] []
                            ]
                    )
                    selectedSquares
                )


findStorageKey : ValidatedFen -> String
findStorageKey validatedFen =
    String.join " " <| List.take 2 <| String.words (Chess.getRaw validatedFen)


toCommand : Position -> String
toCommand position =
    let
        -- TODO: move this function into elm-chess/Chess.Data.Position
        ( row, column ) =
            toRowColumn position
    in
    String.fromChar (Char.fromCode <| column + 97) ++ String.fromInt (8 - row)



---- ACTIONMENU ----


viewActionMenu : Model -> Html Msg
viewActionMenu model =
    div []
        [ div [ H.class "level" ]
            [ case model.initialGameState of
                Nothing ->
                    div [ H.class "level-item" ]
                        [ button [ onClick GetSeed, H.class "button is-primary" ] [ text "Regenerate" ]
                        , button [ onClick Record, H.class "button is-info" ] [ text "Start Recording" ]
                        ]

                Just _ ->
                    div [ H.class "level-item" ]
                        [ button [ H.class "button is-danger", onClick Reset ] [ text "Reset" ]
                        , button (loadingButtonAttributes (onClick SubmitLesson) "is-info" model.submitting) [ text "Submit Lesson" ]
                        ]
            ]
        ]


loadingButtonAttributes : Html.Attribute msg -> String -> Bool -> List (Html.Attribute msg)
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



---- HELPER FUNCTIONS ----


api : String -> String
api apiEndpoint =
    apiEndpoint ++ "/"


findNextSeed : Time.Posix -> Random.Seed
findNextSeed currentTime =
    Random.initialSeed <| Time.posixToMillis currentTime


either : (x -> b) -> (a -> b) -> Result x a -> b
either fromError fromOk result =
    case result of
        Err x ->
            fromError x

        Ok a ->
            fromOk a


isRecording : Maybe ValidatedFen -> Bool
isRecording gameState =
    case gameState of
        Nothing ->
            False

        Just _ ->
            True


fastForward : Int -> Random.Seed -> Random.Seed
fastForward numberOfStepsToApply seed =
    if numberOfStepsToApply == 0 then
        seed

    else
        fastForward (numberOfStepsToApply - 1) <| Tuple.second (Random.step (Random.int 1 2) seed)



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

        validatedFen =
            case result of
                Ok rawFen ->
                    Chess.validateFen rawFen

                Err err ->
                    Err "Validating fen failed from library"
    in
    case validatedFen of
        Ok fen ->
            HandleGameUpdate fen

        Err error ->
            Error ("fen decoding failed: " ++ error)

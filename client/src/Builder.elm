module Builder exposing (main)

-- import AppColor exposing (palette)

import Arithmetic exposing (isEven)
import BuilderJs
import Char
import Chess
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, fieldset, h1, h2, h3, img, input, label, nav, section, span)
import Html.Attributes as H exposing (defaultValue, href, max, min, src, target, type_)
import Html.Events exposing (on, onClick, onInput, targetValue)
import Http exposing (Request, getString, jsonBody)
import Json.Decode as D
import Json.Decode.Pipeline as JDP
import Json.Encode as E exposing (string)
import List.Extra as List
import Mouse
import Piece
import Random.Pcg as Random
import Svg exposing (Svg, g, rect, svg, text, text_)
import Svg.Attributes exposing (fill, fontSize, height, rx, ry, style, viewBox, width, x, y)
import Svg.Events exposing (onMouseDown, onMouseMove, onMouseUp)



---- MODEL ----


type Piece
    = Monarch
    | Hand
    | Rook
    | Bishop
    | Knight
    | Pawn


type Player
    = White
    | Black


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
    , currentGame : Maybe String
    , initialSeed : Random.Seed
    , currentSeed : Random.Seed
    , placements : List Placement
    , pointsAllowed : Int
    , submitting : Bool
    , alerts : List String
    , lessonModel : LessonModel
    , chessModel : Maybe Chess.State
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
            generate initialSeed 1 []
    in
    ( { apiEndpoint = flags.apiEndpoint
      , mode = Basic
      , currentGame = Nothing
      , initialSeed = initialSeed
      , currentSeed = initialSeed
      , placements = initialPlacements
      , pointsAllowed = 1
      , submitting = False
      , alerts = []
      , lessonModel = lessonInit
      , chessModel = Nothing
      }
    , sendPlacements initialPlacements
    )



---- UPDATE ----


type Msg
    = PostLesson
    | HandleGameUpdate String
    | HandleSliderChange Int
    | SelectMode SupportedMode
    | GetSeed
    | FetchSeedCompleted (Result Http.Error Int)
    | PostLessonCompleted (Result Http.Error String)
    | LessonMsg LessonMsg
    | ChessMsg Chess.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        PostLesson ->
            ( { model | submitting = True }, postLessonCmd model )

        HandleGameUpdate fen ->
            ( { model | chessModel = Chess.fromFen fen }, Cmd.none )

        -- ( setFenOnCurrentFrame model fen, Cmd.none )
        HandleSliderChange pointsAllowed ->
            let
                currentSeed =
                    Random.fastForward pointsAllowed model.initialSeed

                placements =
                    generate currentSeed pointsAllowed []
            in
            ( { model
                | currentSeed = currentSeed
                , pointsAllowed = pointsAllowed
                , placements = placements
              }
            , sendPlacements placements
            )

        SelectMode mode ->
            case mode of
                ForcingMoves ->
                    ( { model | mode = mode, alerts = List.concat [ [ "Coming soon! Sorry for the click bait." ], model.alerts ] }, Cmd.none )

                _ ->
                    ( { model | mode = mode, alerts = [] }, Cmd.none )

        GetSeed ->
            ( model, fetchSeedCmd model.apiEndpoint )

        FetchSeedCompleted result ->
            fetchSeedCompleted model result

        PostLessonCompleted result ->
            postLessonCompleted model result

        LessonMsg lessonMsg ->
            let
                ( updatedLessonModel, lessonCmd ) =
                    lessonUpdate lessonMsg model.lessonModel
            in
            ( mapToFen { model | lessonModel = updatedLessonModel }, Cmd.map LessonMsg lessonCmd )

        ChessMsg chessMsg ->
            case model.chessModel of
                Nothing ->
                    ( model, Cmd.none )

                Just chessModel ->
                    let
                        ( updatedChessModel, chessCmd ) =
                            Chess.update { toMsg = ChessMsg, onFenChanged = HandleGameUpdate } chessMsg chessModel
                    in
                    ( { model | chessModel = Just updatedChessModel }, chessCmd )


setFenOnCurrentFrame : Model -> String -> Model
setFenOnCurrentFrame model fen =
    let
        lessonModel =
            model.lessonModel

        selectedFrame =
            lessonModel.selectedFrame

        updatedFrame =
            { selectedFrame | state = fen }

        updatedLessonModel =
            { lessonModel | selectedFrame = updatedFrame }
    in
    { model | currentGame = Just fen, lessonModel = updatedLessonModel }


mapToFen : Model -> Model
mapToFen model =
    model



-- mapToBoard << parseCurrentFrameToFen


parseCurrentFrameToFen : Model -> Model
parseCurrentFrameToFen ({ lessonModel } as model) =
    let
        state =
            lessonModel.selectedFrame.state

        placements =
            parseToPlacements state
    in
    { model | placements = placements }


parseToPlacements : String -> List Placement
parseToPlacements fen =
    let
        maybeFen =
            List.head (String.split " " fen)

        placements =
            case maybeFen of
                Nothing ->
                    []

                Just fen ->
                    parseFenToPlacements fen
    in
    placements


parseFenToPlacements : String -> List Placement
parseFenToPlacements board =
    let
        rows =
            String.split "/" board

        placements =
            List.concat <| List.indexedMap parseFenRowToPlacements rows
    in
    placements


parseFenRowToPlacements : Int -> String -> List Placement
parseFenRowToPlacements columnIndex row =
    String.toList row
        |> List.foldr (parseFenRowHelp columnIndex) ( [], 0 )
        |> Tuple.first


parseFenRowHelp : Int -> Char -> ( List Placement, Int ) -> ( List Placement, Int )
parseFenRowHelp columnIndex square ( placementsSoFar, currentIndex ) =
    let
        ( maybeNextPlacement, offset ) =
            parseSquare square
    in
    case maybeNextPlacement of
        Nothing ->
            ( placementsSoFar, currentIndex + offset )

        Just ( piece, player ) ->
            ( Placement { x = 8 - currentIndex, y = columnIndex + 1 }
                piece
                player
                :: placementsSoFar
            , currentIndex + offset
            )


parseSquare : Char -> ( Maybe ( Piece, Player ), Int )
parseSquare char =
    case Char.isDigit char of
        True ->
            ( Nothing, parseDigit char )

        False ->
            case Dict.get char allPieces of
                Nothing ->
                    ( Nothing, 0 )

                Just ( piece, player ) ->
                    ( Just ( piece, player ), 1 )


allPieces : Dict Char ( Piece, Player )
allPieces =
    Dict.fromList
        [ ( 'P', ( Pawn, White ) )
        , ( 'R', ( Rook, White ) )
        , ( 'N', ( Knight, White ) )
        , ( 'B', ( Bishop, White ) )
        , ( 'Q', ( Hand, White ) )
        , ( 'K', ( Monarch, White ) )
        , ( 'p', ( Pawn, Black ) )
        , ( 'r', ( Rook, Black ) )
        , ( 'n', ( Knight, Black ) )
        , ( 'b', ( Bishop, Black ) )
        , ( 'q', ( Hand, Black ) )
        , ( 'k', ( Monarch, Black ) )
        ]


parseDigit : Char -> Int
parseDigit c =
    Char.toCode c - 48



---- FETCH SEED ----


api : String -> String
api apiEndpoint =
    apiEndpoint ++ "/"


getSeedUrl : String -> String
getSeedUrl apiEndpoint =
    api apiEndpoint ++ "api/seed"


fetchSeed : String -> Http.Request Int
fetchSeed apiEndpoint =
    Http.get (getSeedUrl apiEndpoint) (D.at [ "seed" ] D.int)


fetchSeedCmd : String -> Cmd Msg
fetchSeedCmd apiEndpoint =
    Http.send FetchSeedCompleted (fetchSeed apiEndpoint)


fetchSeedCompleted : Model -> Result Http.Error Int -> ( Model, Cmd Msg )
fetchSeedCompleted model result =
    case Debug.log "fetch new seed result:" result of
        Ok seed ->
            let
                nextSeed =
                    Random.initialSeed seed

                currentSeed =
                    Random.fastForward model.pointsAllowed nextSeed

                placements =
                    generate currentSeed model.pointsAllowed []
            in
            ( { model
                | initialSeed = nextSeed
                , currentSeed = Random.fastForward model.pointsAllowed nextSeed
                , placements = placements
              }
            , sendPlacements placements
            )

        Err _ ->
            ( model, Cmd.none )


postLessonCmd : Model -> Cmd Msg
postLessonCmd { lessonModel, apiEndpoint } =
    let
        body =
            jsonBody
                (E.object
                    [ ( "frames"
                      , frames lessonModel
                      )
                    ]
                )

        request =
            Http.post (api apiEndpoint ++ "api/lesson") body (D.succeed "cake")
    in
    Http.send PostLessonCompleted <| Debug.log "asdffdsa" request


frames : LessonModel -> E.Value
frames model =
    let
        allFrames =
            List.concat [ List.reverse model.previousFrames, [ model.selectedFrame ], model.remainingFrames ]
    in
    E.list (List.map (\f -> frame f) allFrames)


frame : Frame -> E.Value
frame frame =
    E.object
        [ ( "state", E.string frame.state )
        , ( "defaultMessage", E.string frame.defaultMessage )
        , ( "contentMessage", E.string frame.contentMessage )
        ]


postLessonCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
postLessonCompleted model result =
    case Debug.log "post result" result of
        Ok thing ->
            ( { model | submitting = False }, Cmd.none )

        Err _ ->
            ( { model | submitting = False }, Cmd.none )



-- ---- PLACEMENTS ----
-- mapToPlacements : Model -> Model
-- mapToPlacements ({ placements, chessModel } as model) =
--     let
--         updatedPlacements =
--             buildPlacements chessModel.board
--     in
--     { model | placements = updatedPlacements }
-- buildPlacements : Chess.Board -> List Placement
-- buildPlacements board =
--     List.indexedMap
--         (\x row ->
--             List.indexedMap
--                 (\y square ->
--                     case square of
--                         Empty ->
--                             Nothing
--                         Occupied player piece ->
--                             Just
--                                 { piece = piece
--                                 , team = player
--                                 , square = { x = x + 1, y = y + 1 }
--                                 }
--                 )
--                 row
--         )
--         board
--         |> List.concat
--         |> List.filterMap identity
-- mapToBoard : Model -> Model
-- mapToBoard ({ placements, chessModel } as model) =
--     let
--         updatedBoard =
--             buildBoard placements
--         updatedChessModel =
--             { chessModel | board = updatedBoard }
--     in
--     { model | chessModel = updatedChessModel }
-- buildBoard : List Placement -> Board
-- buildBoard placements =
--     List.map (\rowIndex -> buildRank placements rowIndex) (List.range 1 8)
-- buildRank : List Placement -> Int -> Rank
-- buildRank placements rowIndex =
--     List.map (\columnIndex -> buildSquaree placements rowIndex columnIndex) (List.range 1 8)
-- buildSquaree : List Placement -> Int -> Int -> Square
-- buildSquaree placements rowIndex columnIndex =
--     List.filter (\{ square } -> rowIndex == square.x && columnIndex == square.y) placements
--         |> List.head
--         |> Maybe.map (\{ piece, team } -> Occupied team piece)
--         |> Maybe.withDefault Empty


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


generate : Random.Seed -> Int -> List Placement -> List Placement
generate seed pointsAllowed placements =
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
            ( currentPlacements, nextSeed ) =
                generatePlacement pointsAllowed placements seed
        in
        generate nextSeed pointsAllowed currentPlacements


generatePlacement : Int -> List Placement -> Random.Seed -> ( List Placement, Random.Seed )
generatePlacement pointsAllowed placements currentSeed =
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


viewConfig : Chess.ViewConfig
viewConfig =
    { each = "5em", between = "0.15em" }


view : Model -> Html Msg
view model =
    div []
        [ viewNavbar
        , viewAlerts model
        , section [ H.class "section" ]
            [ div [ H.class "container" ]
                [ div [ H.class "columns is-centered is-variable is-8" ]
                    [ div
                        [ H.class "column is-half"
                        ]
                      <|
                        case model.chessModel of
                            Nothing ->
                                [ text "loading" ]

                            Just chessModel ->
                                [ Html.map ChessMsg (Chess.view viewConfig chessModel)
                                , section [ H.class "columns" ]
                                    [ div [ H.class "column is-two-thirds" ] []
                                    , div [ H.class "column is-one-third" ]
                                        [ h2 [ H.class "subtitle is-3" ] [ text <| "Level " ++ toString model.pointsAllowed ]
                                        ]
                                    ]
                                ]
                    , div [ H.class "column is-half has-text-centered" ]
                        [ -- , viewModeSelection model
                          --   Html.map LessonMsg (lessonView model.lessonModel)
                          -- , viewKitty model
                          h3 [ H.class "subtitle is-5" ] [ text "Material" ]
                        , makeSlider model
                        , viewActionMenu model
                        , viewCurrentGame model
                        ]
                    ]
                ]
            ]
        ]


viewCurrentGame : Model -> Html Msg
viewCurrentGame { currentGame } =
    h2 [ H.class "is-hidden" ]
        [ case currentGame of
            Nothing ->
                text ""

            Just game ->
                text game
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



---- KITTY ----
-- viewKitty : Model -> Html Msg
-- viewKitty model =
--     div [ H.class "box" ]
--         ([ h3 [ H.class "subtitle is-5" ] [ text "Available Pieces" ] ]
--             ++ List.map (\team -> viewKittyTeam model team) [ White, Black ]
--         )
-- viewKittyTeam : Model -> Player -> Html Msg
-- viewKittyTeam model team =
--     let
--         pointsDeployed =
--             List.foldr (\placement result -> result + findPointValueFromPiece placement.piece) 0 model.placements
--     in
--     div [ H.class "level" ]
--         (List.map
--             (\piece ->
--                 let
--                     isDisabled =
--                         model.pointsAllowed < pointsDeployed + findPointValueFromPiece piece
--                 in
--                 div [ H.class "level-item" ]
--                     [ button
--                         [ H.class "button is-white"
--                         , H.disabled isDisabled
--                         , H.style [ ( "height", "100%" ) ]
--                         ]
--                         [ kittyPieceView piece team ]
--                     ]
--             )
--             kittyPieces
--         )
-- kittyPieces : List Piece
-- kittyPieces =
--     [ Hand, Rook, Bishop, Knight, Pawn ]
-- kittyPieceView : Piece -> Player -> Svg Msg
-- kittyPieceView piece player =
--     pieceView piece player [] (toFloat <| 5 // 2) (toFloat <| 5 // 2)
-- pieceView piece player [] (toFloat <| squareSize // 2) (toFloat <| squareSize // 2)
---- ACTIONMENU ----


viewActionMenu : Model -> Html Msg
viewActionMenu model =
    div []
        [ div [ H.class "level" ]
            [ div [ H.class "level-item" ] [ button [ onClick GetSeed, H.class "button is-primary" ] [ text "Start Over" ] ]
            , div [ H.class "level-item" ]
                [ button (loadingButtonAttributes (onClick PostLesson) "is-info" model.submitting)
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
    Sub.batch <|
        case model.chessModel of
            Nothing ->
                [ BuilderJs.fromJs decodeFen ]

            Just chessModel ->
                [ Sub.map ChessMsg (Chess.subscriptions chessModel), BuilderJs.fromJs decodeFen ]


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



-- ---- CHESS ----
-- ---- CHESSMODEL ----
-- type alias ChessModel =
--     { board : Board
--     , drag : Maybe Drag
--     , mousePosition : Mouse.Position
--     }
-- type Drag
--     = Drag Player Piece
-- type Player
--     = White
--     | Black
-- type Square
--     = Empty
--     | Occupied Player Piece
-- type Location
--     = Location Int Int
-- type alias Board =
--     List Rank
-- type alias Rank =
--     List Square
-- type alias MouseMove =
--     { offsetX : Int
--     , offsetY : Int
--     }
-- chessInit : ChessModel
-- chessInit =
--     { board = newGame
--     , drag = Nothing
--     , mousePosition = { x = 0, y = 0 }
--     }
-- newGame : Board
-- newGame =
--     List.repeat 8 (List.repeat 8 Empty)
-- ---- CHESSUPDATE ----
-- type ChessMsg
--     = NoOp
--     | DragStart Player Piece Location
--     | DragEnd Location
--     | MouseMoved MouseMove
-- chessUpdate : ChessMsg -> ChessModel -> ( ChessModel, Cmd ChessMsg )
-- chessUpdate msg model =
--     case msg of
--         NoOp ->
--             ( model, Cmd.none )
--         DragStart player piece location ->
--             let
--                 updatedBoard =
--                     emptySquare location model.board
--             in
--             ( { model | board = updatedBoard, drag = Just (Drag player piece) }, Cmd.none )
--         DragEnd location ->
--             let
--                 updatedBoard =
--                     placePiece location model.drag model.board
--             in
--             ( { model | board = updatedBoard, drag = Nothing }, Cmd.none )
--         MouseMoved { offsetX, offsetY } ->
--             ( { model | mousePosition = { x = offsetX, y = offsetY } }, Cmd.none )
-- emptySquare : Location -> Board -> Board
-- emptySquare (Location rankIndex fileIndex) board =
--     List.updateAt rankIndex (\rank -> emptySquareInRank rank fileIndex) board
-- emptySquareInRank : Rank -> Int -> Rank
-- emptySquareInRank rank fileIndex =
--     List.updateAt fileIndex (\_ -> Empty) rank
-- placePiece : Location -> Maybe Drag -> Board -> Board
-- placePiece (Location rankIndex fileIndex) drag board =
--     case drag of
--         Nothing ->
--             board
--         Just (Drag player piece) ->
--             List.updateAt rankIndex (\rank -> List.updateAt fileIndex (\_ -> Occupied player piece) rank) board
-- ---- CHESSVIEW ----
-- chessView : ChessModel -> Html ChessMsg
-- chessView model =
--     svg
--         [ width (toString boardSize), height (toString boardSize), viewBox boardViewBox ]
--         [ boardView model.board
--         , dragView model
--         ]
-- boardView : Board -> Svg ChessMsg
-- boardView board =
--     g [ onMouseMove MouseMoved ]
--         (List.indexedMap rankView board)
-- onMouseMove : (MouseMove -> ChessMsg) -> Svg.Attribute ChessMsg
-- onMouseMove callback =
--     Svg.Events.on "mousemove" (D.map callback mouseMoveDecoder)
-- mouseMoveDecoder : D.Decoder MouseMove
-- mouseMoveDecoder =
--     JDP.decode MouseMove
--         |> JDP.required "offsetX" D.int
--         |> JDP.required "offsetY" D.int
-- dragView : ChessModel -> Svg ChessMsg
-- dragView { drag, mousePosition } =
--     case drag of
--         Nothing ->
--             Svg.text ""
--         Just (Drag player piece) ->
--             pieceView piece player [ style "pointer-events: none;" ] (toFloat mousePosition.x) (toFloat mousePosition.y)
-- boardViewBox : String
-- boardViewBox =
--     [ 0, 0, boardSize, boardSize ]
--         |> List.map toString
--         |> String.join " "
-- rankView : Int -> Rank -> Svg ChessMsg
-- rankView rankIndex rank =
--     g [] (List.indexedMap (squareView rankIndex) rank)
-- squareView : Int -> Int -> Square -> Svg ChessMsg
-- squareView rankIndex fileIndex square =
--     svg
--         [ x (toString <| rankIndex * squareSize)
--         , y (toString <| (7 - fileIndex) * squareSize)
--         , width <| toString <| squareSize
--         , height <| toString <| squareSize
--         , onMouseUp (DragEnd (Location rankIndex fileIndex))
--         ]
--         [ squareFillView rankIndex fileIndex square
--         , coordinateAnnotationView rankIndex fileIndex
--         , squarePieceView square (Location rankIndex fileIndex)
--         ]
-- squarePieceView : Square -> Location -> Svg ChessMsg
-- squarePieceView square location =
--     case square of
--         Empty ->
--             g [] []
--         Occupied player piece ->
--             pieceView piece player [ onMouseDown (DragStart player piece location) ] (toFloat <| squareSize // 2) (toFloat <| squareSize // 2)
-- pieceView : Piece -> Player -> (List (Svg.Attribute msg) -> Float -> Float -> Svg msg)
-- pieceView piece player attrs left top =
--     case piece of
--         Pawn ->
--             case player of
--                 Black ->
--                     Piece.blackPawn attrs left top
--                 White ->
--                     Piece.whitePawn attrs left top
--         Bishop ->
--             case player of
--                 Black ->
--                     Piece.blackBishop attrs left top
--                 White ->
--                     Piece.whiteBishop attrs left top
--         Knight ->
--             case player of
--                 Black ->
--                     Piece.blackKnight attrs left top
--                 White ->
--                     Piece.whiteKnight attrs left top
--         Monarch ->
--             case player of
--                 Black ->
--                     Piece.blackKing attrs left top
--                 White ->
--                     Piece.whiteKing attrs left top
--         Hand ->
--             case player of
--                 Black ->
--                     Piece.blackQueen attrs left top
--                 White ->
--                     Piece.whiteQueen attrs left top
--         Rook ->
--             case player of
--                 Black ->
--                     Piece.blackRook attrs left top
--                 White ->
--                     Piece.whiteRook attrs left top
-- squareFillView : Int -> Int -> Square -> Svg ChessMsg
-- squareFillView rankIndex fileIndex square =
--     rect
--         [ width (toString squareSize)
--         , height (toString squareSize)
--         , fill <| squareColor rankIndex fileIndex
--         ]
--         []
-- squareColor : Int -> Int -> String
-- squareColor rankIndex fileIndex =
--     if isEven (rankIndex + fileIndex) then
--         Chess.palette.purple
--     else
--         palette.gray
-- coordinateAnnotationView : Int -> Int -> Svg ChessMsg
-- coordinateAnnotationView rankIndex fileIndex =
--     g [] <|
--         List.filterMap identity <|
--             [ if fileIndex == 0 then
--                 Just <| letterView rankIndex
--               else
--                 Nothing
--             , if rankIndex == 0 then
--                 Just <| numberView fileIndex
--               else
--                 Nothing
--             ]
-- letterView : Int -> Svg ChessMsg
-- letterView rankIndex =
--     text_
--         [ fontSize <| toString <| coordsFontSize
--         , x <| toString <| (squareSize - coordsFontSize)
--         , y <| toString <| (8 + squareSize - coordsFontSize)
--         ]
--         [ text (indexToRank rankIndex) ]
-- coordsFontSize : Int
-- coordsFontSize =
--     14
-- numberView : Int -> Svg ChessMsg
-- numberView fileIndex =
--     text_
--         [ fontSize <| toString <| coordsFontSize
--         , x "5"
--         , y "18"
--         ]
--         [ text <| toString <| fileIndex + 1 ]
-- boardSize : Int
-- boardSize =
--     600
-- squareSize : Int
-- squareSize =
--     boardSize // 8
-- indexToRank : Int -> String
-- indexToRank index =
--     [ "a", "b", "c", "d", "e", "f", "g", "h" ]
--         |> List.getAt index
--         |> Maybe.withDefault ""
---- LESSON SYSTEM ----


type alias Frame =
    { state : String
    , defaultMessage : String
    , contentMessage : String

    -- , squareToMessage : Dict.fromList \
    --     [ "square" Square (Maybe String)
    }


squares =
    [ "a1"
    , "a2"
    , "a3"
    , "a4"
    , "a6"
    , "a7"
    , "a8"
    , "b1"
    , "b2"
    , "b3"
    , "b4"
    , "b6"
    , "b7"
    , "c8"
    , "c1"
    , "c2"
    , "c3"
    , "c4"
    , "c6"
    , "c7"
    , "c8"
    , "d1"
    , "d2"
    , "d3"
    , "d4"
    , "d6"
    , "d7"
    , "d8"
    , "e1"
    , "e2"
    , "e3"
    , "e4"
    , "e6"
    , "e7"
    , "e8"
    , "f1"
    , "f2"
    , "f3"
    , "f4"
    , "f6"
    , "f7"
    , "f8"
    , "g1"
    , "g2"
    , "g3"
    , "g4"
    , "g6"
    , "g7"
    , "g8"
    , "h1"
    , "h2"
    , "h3"
    , "h4"
    , "h6"
    , "h7"
    , "h8"
    ]


type alias LessonModel =
    { previousFrames : List Frame
    , selectedFrame : Frame
    , remainingFrames : List Frame
    }



-- previousFrames needs to be reversed maybe.


lessonInit : LessonModel
lessonInit =
    { previousFrames = []
    , selectedFrame = emptyFrame
    , remainingFrames = []
    }


emptyFrame : Frame
emptyFrame =
    Frame
        ""
        ""
        ""



---- LESSONUPDATE ----


type LessonMsg
    = Default String
    | Content String
    | NextFrame
    | PreviousFrame


lessonUpdate : LessonMsg -> LessonModel -> ( LessonModel, Cmd LessonMsg )
lessonUpdate msg model =
    case Debug.log "Lesson.update" msg of
        Default default ->
            ( updateDefault model default, Cmd.none )

        Content content ->
            ( updateContent model content, Cmd.none )

        NextFrame ->
            ( nextFrame model, Cmd.none )

        PreviousFrame ->
            ( previousFrame model, Cmd.none )


updateDefault : LessonModel -> String -> LessonModel
updateDefault model default =
    let
        frame =
            model.selectedFrame

        updatedFrame =
            { frame | defaultMessage = default }
    in
    { model | selectedFrame = updatedFrame }


updateContent : LessonModel -> String -> LessonModel
updateContent model content =
    let
        frame =
            model.selectedFrame

        updatedFrame =
            { frame | contentMessage = content }
    in
    { model | selectedFrame = updatedFrame }


nextFrame : LessonModel -> LessonModel
nextFrame model =
    let
        ( previous, current, remaining ) =
            case model.remainingFrames of
                [] ->
                    ( model.selectedFrame :: model.previousFrames, emptyFrame, [] )

                frame :: remaining ->
                    ( model.selectedFrame :: model.previousFrames, frame, remaining )
    in
    { model | previousFrames = previous, selectedFrame = current, remainingFrames = remaining }


previousFrame : LessonModel -> LessonModel
previousFrame model =
    let
        ( previous, current, remaining ) =
            case model.previousFrames of
                frame :: previous ->
                    ( previous, frame, model.selectedFrame :: model.remainingFrames )

                [] ->
                    ( model.previousFrames, model.selectedFrame, model.remainingFrames )
    in
    { model | previousFrames = previous, selectedFrame = current, remainingFrames = remaining }



--- LESSONVIEW ----
-- Disable back button if not applicable.


lessonView : LessonModel -> Html LessonMsg
lessonView model =
    div []
        [ h3 [ H.class "subtitle is-5" ] [ text "Lesson" ]
        , div [] [ text (toString model.selectedFrame) ]
        , div [] [ input [ H.class "input", type_ "text", H.placeholder "Default", H.value model.selectedFrame.defaultMessage, onInput Default ] [] ]
        , div [] [ input [ H.class "input", type_ "text", H.placeholder "Lesson", H.value model.selectedFrame.contentMessage, onInput Content ] [] ]
        , div []
            [ span [ H.class "button", H.property "innerHTML" (string "&#60;"), onClick PreviousFrame ] []
            , span [ H.class "button", H.property "innerHTML" (string "&#62;"), onClick NextFrame ] []
            ]
        ]



-- , div [] [ input [ H.class "input", type_ "text", H.defaultValue "When you march a pawn to the end you may promote it", H.placeholder "Hint", onInput Content ] [] ]
-- , div [] [ input [ H.class "input", type_ "text", H.defaultValue "Try clicking a green square", H.placeholder "Lesson", onInput Default ] [] ]

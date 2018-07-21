module Main exposing (..)

import AppColor exposing (palette)
import Arithmetic exposing (isEven)
import Html exposing (Html, a, button, div, h1, h2, img, input, nav, section, span)
import Html.Attributes as H exposing (defaultValue, href, max, min, src, target, type_)
import Html.Events exposing (on, onClick, targetValue)
import Js
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


type alias Model =
    { currentGame : Maybe String
    , currentSeed : Random.Seed
    , placements : List Placement
    , pointsAllowed : Int
    , chessModel : ChessModel
    }


init : ( Model, Cmd Msg )
init =
    ( generate
        { currentGame = Nothing
        , currentSeed = Random.initialSeed 12345
        , placements = []
        , pointsAllowed = 22
        , chessModel = chessInit
        }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Validate
    | HandleGameUpdate String
    | HandleSliderChange Int
    | ChessMsg ChessMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Validate ->
            ( model, sendPlacements model.placements )

        HandleGameUpdate fen ->
            ( { model | currentGame = Just fen }, Cmd.none )

        HandleSliderChange pointsAllowed ->
            ( generate { model | pointsAllowed = pointsAllowed, placements = [] }, Cmd.none )

        ChessMsg chessMsg ->
            let
                ( updatedChessModel, chessCmd ) =
                    chessUpdate chessMsg model.chessModel
            in
            ( mapToPlacements { model | chessModel = updatedChessModel }, Cmd.map ChessMsg chessCmd )


mapToPlacements : Model -> Model
mapToPlacements ({ placements, chessModel } as model) =
    let
        updatedPlacements =
            buildPlacements chessModel.board
    in
    { model | placements = updatedPlacements }


buildPlacements : Board -> List Placement
buildPlacements board =
    List.indexedMap
        (\x row ->
            List.indexedMap
                (\y square ->
                    case square of
                        Empty ->
                            Nothing

                        Occupied player piece ->
                            Just
                                { piece = piece
                                , team = player
                                , square = { x = x + 1, y = y + 1 }
                                }
                )
                row
        )
        board
        |> List.concat
        |> List.filterMap identity


mapToBoard : Model -> Model
mapToBoard ({ placements, chessModel } as model) =
    let
        updatedBoard =
            buildBoard placements

        updatedChessModel =
            { chessModel | board = updatedBoard }
    in
    { model | chessModel = updatedChessModel }


buildBoard : List Placement -> Board
buildBoard placements =
    List.map (\rowIndex -> buildRank placements rowIndex) (List.range 1 8)


buildRank : List Placement -> Int -> Rank
buildRank placements rowIndex =
    List.map (\columnIndex -> buildSquaree placements rowIndex columnIndex) (List.range 1 8)


buildSquaree : List Placement -> Int -> Int -> Square
buildSquaree placements rowIndex columnIndex =
    List.filter (\{ square } -> rowIndex == square.x && columnIndex == square.y) placements
        |> List.head
        |> Maybe.map (\{ piece, team } -> Occupied team piece)
        |> Maybe.withDefault Empty


sendPlacements : List Placement -> Cmd msg
sendPlacements placements =
    Js.fromElm
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


generate : Model -> Model
generate model =
    let
        hasBothMonarchs =
            2 == List.length (List.filter (is Monarch) model.placements)

        atMaxScore =
            currentTotal model.placements == model.pointsAllowed
    in
    if hasBothMonarchs && atMaxScore then
        mapToBoard model
    else
        generate <| generatePlacement model


generatePlacement : Model -> Model
generatePlacement model =
    let
        ( selectedSquare, updatedSeed ) =
            Random.step squareGenerator model.currentSeed

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
    { model
        | currentSeed = evenMoarUpdatedSeed
        , placements = conditionalUpdatedBoard model.pointsAllowed model.placements constructed
    }


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
        , section [ H.class "section" ]
            [ div [ H.class "container columns" ]
                [ div
                    [ H.class "column is-three-quarters"
                    ]
                    [ Html.map ChessMsg (chessView model.chessModel) ]
                , div [ H.class "column" ]
                    [ viewKitty model
                    , makeSlider model
                    , text <| toString model.pointsAllowed
                    , div [] [ button [ onClick Validate, H.class "button is-info" ] [ text "Submit Lesson" ] ]
                    ]
                ]
            ]
        ]


viewNavbar : Html Msg
viewNavbar =
    nav
        [ H.class "navbar has-shadow is-spaced" ]
        [ div [ H.class "navbar-brand" ]
            [ a
                [ H.class "navbar-item"
                , H.href "https://github.com/7hoenix/procedural-gen"
                , H.target "_blank"
                ]
                [ h1 [] [ text "Builder" ] ]
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
            [ div [ H.class "navbar-start" ] []
            , div [ H.class "navbar-end" ] []
            ]
        ]


makeSlider : Model -> Html Msg
makeSlider model =
    input
        [ type_ "range"
        , H.min "1"
        , H.max "22"
        , defaultValue <| toString model.pointsAllowed
        , on "input" (targetValue |> D.andThen parseInt)
        ]
        []



---- KITTY ----


viewKitty : Model -> Html Msg
viewKitty model =
    div [ H.class "box" ]
        (List.map (\team -> viewKittyTeam model team) [ White, Black ])


viewKittyTeam : Model -> Player -> Html Msg
viewKittyTeam model team =
    let
        pointsDeployed =
            Debug.log "hi" (List.foldr (\placement result -> result + findPointValueFromPiece placement.piece) 0 model.placements)
    in
    div [ H.class "level" ]
        (List.map
            (\piece ->
                let
                    isDisabled =
                        model.pointsAllowed < pointsDeployed + findPointValueFromPiece piece
                in
                div [ H.class "level-item" ]
                    [ button
                        [ H.class "button is-white"
                        , H.disabled isDisabled
                        , H.style [ ( "height", "100%" ) ]
                        ]
                        [ kittyPieceView piece team ]
                    ]
            )
            kittyPieces
        )


kittyPieces : List Piece
kittyPieces =
    [ Hand, Rook, Bishop, Knight, Pawn ]


kittyPieceView : Piece -> Player -> Svg Msg
kittyPieceView piece player =
    pieceView piece player [] (toFloat <| squareSize // 2) (toFloat <| squareSize // 2)


parseInt : String -> D.Decoder Msg
parseInt rawString =
    case String.toInt rawString of
        Err err ->
            D.fail err

        Ok int ->
            D.succeed (HandleSliderChange int)


displayConstructed : List Placement -> Html Msg
displayConstructed constructedElements =
    List.foldr (\element result -> result ++ " " ++ toString element.square ++ " \x0D\n " ++ toString element.piece ++ " " ++ toString element.team) "" constructedElements
        |> text


displayGame : Maybe String -> Html Msg
displayGame maybeFen =
    case maybeFen of
        Nothing ->
            text "No game here"

        Just fen ->
            text fen



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Js.fromJs decodeFen


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


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }



---- CHESS ----
---- CHESSMODEL ----


type alias ChessModel =
    { board : Board
    , drag : Maybe Drag
    , mousePosition : Mouse.Position
    }


type Drag
    = Drag Player Piece


type Player
    = White
    | Black


type Square
    = Empty
    | Occupied Player Piece


type Location
    = Location Int Int


type alias Board =
    List Rank


type alias Rank =
    List Square


type alias MouseMove =
    { offsetX : Int
    , offsetY : Int
    }


chessInit : ChessModel
chessInit =
    { board = newGame
    , drag = Nothing
    , mousePosition = { x = 0, y = 0 }
    }


newGame : Board
newGame =
    List.repeat 8 (List.repeat 8 Empty)



---- CHESSUPDATE ----


type ChessMsg
    = NoOp
    | DragStart Player Piece Location
    | DragEnd Location
    | MouseMoved MouseMove


chessUpdate : ChessMsg -> ChessModel -> ( ChessModel, Cmd ChessMsg )
chessUpdate msg model =
    case Debug.log "Main.update" msg of
        NoOp ->
            ( model, Cmd.none )

        DragStart player piece location ->
            let
                updatedBoard =
                    emptySquare location model.board
            in
            ( { model | board = updatedBoard, drag = Just (Drag player piece) }, Cmd.none )

        DragEnd location ->
            let
                updatedBoard =
                    placePiece location model.drag model.board
            in
            ( { model | board = updatedBoard, drag = Nothing }, Cmd.none )

        MouseMoved { offsetX, offsetY } ->
            ( { model | mousePosition = { x = offsetX, y = offsetY } }, Cmd.none )


emptySquare : Location -> Board -> Board
emptySquare (Location rankIndex fileIndex) board =
    List.updateAt rankIndex (\rank -> emptySquareInRank rank fileIndex) board


emptySquareInRank : Rank -> Int -> Rank
emptySquareInRank rank fileIndex =
    List.updateAt fileIndex (\_ -> Empty) rank


placePiece : Location -> Maybe Drag -> Board -> Board
placePiece (Location rankIndex fileIndex) drag board =
    case drag of
        Nothing ->
            board

        Just (Drag player piece) ->
            List.updateAt rankIndex (\rank -> List.updateAt fileIndex (\_ -> Occupied player piece) rank) board



---- CHESSVIEW ----


chessView : ChessModel -> Html ChessMsg
chessView model =
    svg
        [ width (toString boardSize), height (toString boardSize), viewBox boardViewBox ]
        [ boardView model.board
        , dragView model
        ]


boardView : Board -> Svg ChessMsg
boardView board =
    g [ onMouseMove MouseMoved ]
        (List.indexedMap rankView (Debug.log "board" board))


onMouseMove : (MouseMove -> ChessMsg) -> Svg.Attribute ChessMsg
onMouseMove callback =
    Svg.Events.on "mousemove" (D.map callback mouseMoveDecoder)


mouseMoveDecoder : D.Decoder MouseMove
mouseMoveDecoder =
    JDP.decode MouseMove
        |> JDP.required "offsetX" D.int
        |> JDP.required "offsetY" D.int


dragView : ChessModel -> Svg ChessMsg
dragView { drag, mousePosition } =
    case drag of
        Nothing ->
            Svg.text ""

        Just (Drag player piece) ->
            pieceView piece player [ style "pointer-events: none;" ] (toFloat mousePosition.x) (toFloat mousePosition.y)


boardViewBox : String
boardViewBox =
    [ 0, 0, boardSize, boardSize ]
        |> List.map toString
        |> String.join " "


rankView : Int -> Rank -> Svg ChessMsg
rankView rankIndex rank =
    g [] (List.indexedMap (squareView rankIndex) rank)


squareView : Int -> Int -> Square -> Svg ChessMsg
squareView rankIndex fileIndex square =
    svg
        [ x (toString <| rankIndex * squareSize)
        , y (toString <| (7 - fileIndex) * squareSize)
        , width <| toString <| squareSize
        , height <| toString <| squareSize
        , onMouseUp (DragEnd (Location rankIndex fileIndex))
        ]
        [ squareFillView rankIndex fileIndex square
        , coordinateAnnotationView rankIndex fileIndex
        , squarePieceView square (Location rankIndex fileIndex)
        ]


squarePieceView square location =
    case square of
        Empty ->
            g [] []

        Occupied player piece ->
            pieceView piece player [ onMouseDown (DragStart player piece location) ] (toFloat <| squareSize // 2) (toFloat <| squareSize // 2)


pieceView : Piece -> Player -> (List (Svg.Attribute msg) -> Float -> Float -> Svg msg)
pieceView piece player attrs left top =
    case piece of
        Pawn ->
            case player of
                Black ->
                    Piece.blackPawn attrs left top

                White ->
                    Piece.whitePawn attrs left top

        Bishop ->
            case player of
                Black ->
                    Piece.blackBishop attrs left top

                White ->
                    Piece.whiteBishop attrs left top

        Knight ->
            case player of
                Black ->
                    Piece.blackKnight attrs left top

                White ->
                    Piece.whiteKnight attrs left top

        Monarch ->
            case player of
                Black ->
                    Piece.blackKing attrs left top

                White ->
                    Piece.whiteKing attrs left top

        Hand ->
            case player of
                Black ->
                    Piece.blackQueen attrs left top

                White ->
                    Piece.whiteQueen attrs left top

        Rook ->
            case player of
                Black ->
                    Piece.blackRook attrs left top

                White ->
                    Piece.whiteRook attrs left top


squareFillView : Int -> Int -> Square -> Svg ChessMsg
squareFillView rankIndex fileIndex square =
    rect
        [ width (toString squareSize)
        , height (toString squareSize)
        , fill <| squareColor rankIndex fileIndex
        ]
        []


squareColor : Int -> Int -> String
squareColor rankIndex fileIndex =
    if isEven (rankIndex + fileIndex) then
        palette.purple
    else
        palette.gray


coordinateAnnotationView : Int -> Int -> Svg ChessMsg
coordinateAnnotationView rankIndex fileIndex =
    g [] <|
        List.filterMap identity <|
            [ if fileIndex == 0 then
                Just <| letterView rankIndex
              else
                Nothing
            , if rankIndex == 0 then
                Just <| numberView fileIndex
              else
                Nothing
            ]


letterView : Int -> Svg ChessMsg
letterView rankIndex =
    text_
        [ fontSize <| toString <| coordsFontSize
        , x <| toString <| (squareSize - coordsFontSize)
        , y <| toString <| (8 + squareSize - coordsFontSize)
        ]
        [ text (indexToRank rankIndex) ]


coordsFontSize =
    14


numberView : Int -> Svg ChessMsg
numberView fileIndex =
    text_
        [ fontSize <| toString <| coordsFontSize
        , x "5"
        , y "18"
        ]
        [ text <| toString <| fileIndex + 1 ]


boardSize =
    600


squareSize =
    boardSize // 8


indexToRank index =
    [ "a", "b", "c", "d", "e", "f", "g", "h" ]
        |> List.getAt index
        |> Maybe.withDefault ""

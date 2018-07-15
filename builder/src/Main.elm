module Main exposing (..)

import Arithmetic exposing (isEven)
import Html exposing (Html, button, div, h1, h2, img)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
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
-- Monarch can't be placed in player check


type Piece
    = Monarch
    | Hand
    | Rook
    | Bishop
    | Knight
    | Pawn


type Team
    = Player
    | Opponent


type alias SquareLocation =
    { x : Int, y : Int }


type alias Placement =
    { square : SquareLocation
    , piece : Piece
    , team : Team
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
    ( { currentGame = Nothing
      , currentSeed = Random.initialSeed 12345
      , placements = []
      , pointsAllowed = 6
      , chessModel = chessInit
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Generate
    | GeneratePlacement
    | Validate
    | HandleGameUpdate String
    | ChessMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( generate model, Cmd.none )

        GeneratePlacement ->
            ( generatePlacement model, Cmd.none )

        Validate ->
            ( model, sendPlacements model.placements )

        HandleGameUpdate fen ->
            ( { model | currentGame = Just fen }, Cmd.none )

        ChessMsg ->
            ( model, Cmd.none )


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


team : Team -> E.Value
team team =
    case team of
        Player ->
            E.string "b"

        Opponent ->
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
        model
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
    case ( findMonarch Player, findMonarch Opponent ) of
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


teamGenerator : Random.Generator Team
teamGenerator =
    Random.choice Player Opponent



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text (toString model.currentSeed) ]
        , h2 [] [ displayConstructed model.placements ]
        , button [ onClick Generate ] [ text "Generate content" ]
        , button [ onClick Validate ] [ text "Validate position" ]
        , h1 [] [ displayGame model.currentGame ]
        ]


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



-- type Piece
--     = Pawn
--     | Knight
--     | Bishop
--     | Rook
--     | Queen
--     | King


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
    List.transpose <|
        List.reverse <|
            [ [ Occupied Black Rook, Occupied Black Knight, Occupied Black Bishop, Occupied Black Hand, Occupied Black Monarch, Occupied Black Bishop, Occupied Black Knight, Occupied Black Rook ]
            , [ Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn, Occupied Black Pawn ]
            , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
            , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
            , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
            , [ Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty ]
            , [ Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn, Occupied White Pawn ]
            , [ Occupied White Rook, Occupied White Knight, Occupied White Bishop, Occupied White Hand, Occupied White Monarch, Occupied White Bishop, Occupied White Knight, Occupied White Rook ]
            ]



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
        "#D8D2E1"
    else
        "#34435E"


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

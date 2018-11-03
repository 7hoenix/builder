module Chess exposing
    ( Msg
    , State
    , fromFen
    , subscriptions
    , update
    , view
    , ValidatedFen, blankState, blankValidatedFen, getBoard, getRaw, getSquaresSelected, getTeam, setTeam, validateFen
    )

{-|

@docs Msg
@docs State
@docs fromFen
@docs subscriptions
@docs update
@docs view

-}

import Animation
import Browser.Dom exposing (getElement)
import Chess.Data.Board exposing (Board, Square(..), blankBoard)
import Chess.Data.Piece exposing (Piece(..))
import Chess.Data.Player exposing (Player(..))
import Chess.Data.Position exposing (Position)
import Chess.View.Asset
import Chess.View.Board
import Drag
import Html exposing (Attribute, Html)
import Html.Attributes
import Html.Events exposing (..)
import Json.Decode as D
import Json.Encode as E
import Task



---- MODEL ----


{-| -}
type State
    = State
        { board : Board
        , team : Player
        , drag : Drag.State DraggableItem
        , hover : Maybe Position
        , squaresSelected : List Position
        }


type alias DraggableItem =
    { position : Position
    , player : Player
    , piece : Piece
    }


type ValidatedFen
    = ValidatedFen Board Player String



-- type Turn
--     = FirstToAct
--     | SecondToAct


type IntermediateFen
    = Raw String String String
    | IBoard Board String String
    | IBoardPlusTurn Board Player String


{-|

    This is the primary entry point for the app.

    This is its type signature:

    validateFen : String -> Result String ValidatedFen


    It takes a fen string as input (fen stands for (Forsyth-Edwards Notation)[https://en.wikipedia.org/wiki/Forsyth–Edwards_Notation#Examples]. Fen
    is the most succint way I've found to represent a full game of chess state).


    This is the starting position for a chess game for instance: "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".


    To get a ValidatedFen type you must first take the raw string and pipe it through this function.
    Then define what happens if you get an error back from the function and then use the type as you intended.

-}
validateFen : String -> Result String ValidatedFen
validateFen rawFen =
    let
        fenResult : String -> Result String IntermediateFen
        fenResult superRaw =
            case String.split " " superRaw of
                [ a, b, c, d, e, f ] ->
                    Ok <| Raw a b superRaw

                _ ->
                    Err <| rawFen ++ " doesn't look right. FEN needs to have 6 pieces of info"

        validateBoard : IntermediateFen -> Result String IntermediateFen
        validateBoard inter =
            case inter of
                Raw rawBoard b baseRaw ->
                    case D.decodeValue Chess.Data.Board.boardDecoder3 (E.string rawBoard) of
                        Ok board ->
                            Ok <| IBoard board b baseRaw

                        Err reason ->
                            Err "Board validation failure."

                _ ->
                    Err "Called out of order"

        validateTurn : IntermediateFen -> Result String IntermediateFen
        validateTurn inter =
            case inter of
                IBoard board rawTurn baseRaw ->
                    case parseTurn rawTurn of
                        Ok turn ->
                            Ok <| IBoardPlusTurn board turn baseRaw

                        Err reason ->
                            Err reason

                _ ->
                    Err "Called out of order"

        validatedFen =
            fenResult rawFen
                |> Result.andThen validateBoard
                |> Result.andThen validateTurn
    in
    case validatedFen of
        Ok (IBoardPlusTurn board turn baseRaw) ->
            Ok <| ValidatedFen board turn baseRaw

        _ ->
            Err "Called out of order maybe?"


{-| -}
fromFen : ValidatedFen -> State
fromFen (ValidatedFen board turn _) =
    State
        { board = board
        , team = turn
        , hover = Nothing
        , drag =
            { subject = Nothing
            , position = ( 0, 0 )
            , original = Animation.style present
            , cursor = Animation.style gone
            }
        , squaresSelected = []
        }


blankState : State
blankState =
    State
        { board = blankBoard
        , team = White
        , hover = Nothing
        , drag =
            { subject = Nothing
            , position = ( 0, 0 )
            , original = Animation.style present
            , cursor = Animation.style gone
            }
        , squaresSelected = []
        }


blankValidatedFen : ValidatedFen
blankValidatedFen =
    ValidatedFen blankBoard White "8/8/8/8/8/8/8/8 w - - 0 0"


getBoard : ValidatedFen -> Board
getBoard (ValidatedFen board _ _) =
    board


getTeam : ValidatedFen -> Player
getTeam (ValidatedFen _ player _) =
    player


setTeam : Player -> ValidatedFen -> ValidatedFen
setTeam updatedTeam (ValidatedFen board _ raw) =
    ValidatedFen board updatedTeam raw


getRaw : ValidatedFen -> String
getRaw (ValidatedFen _ _ raw) =
    raw


{-| -}
parseTurn : String -> Result String Player
parseTurn turn =
    case turn of
        "w" ->
            Ok White

        "b" ->
            Ok Black

        _ ->
            Err <| turn ++ " is not a valid entry. do 'w' for FirstToAct or 'b' for SecondToAct"


{-| -}
getSquaresSelected : State -> List Position
getSquaresSelected (State state) =
    state.squaresSelected



--- UPDATE ----


{-| -}
type Msg
    = DragMsg (Drag.Msg DraggableItem)
    | FinalRelease DraggableItem
    | SelectSquare Position
    | SetHover Position


type alias Config msg =
    { toMsg : Msg -> msg
    , onFenChanged : ValidatedFen -> msg
    , isRecording : Bool
    }


{-| -}
update : Config msg -> Msg -> State -> ( State, Cmd msg )
update config msg (State state) =
    case msg of
        DragMsg dragMsg ->
            Drag.update dragConfig dragMsg state.drag
                |> Tuple.mapFirst (\drag -> State { state | drag = drag })
                |> Tuple.mapSecond (Cmd.map config.toMsg)

        FinalRelease from ->
            case state.hover of
                Nothing ->
                    ( State state, Cmd.none )

                Just to ->
                    let
                        board =
                            makeMove from to state.board

                        updatedTeam =
                            if config.isRecording then
                                nextTeam state.team

                            else
                                state.team

                        rawFen =
                            Chess.Data.Board.toFen board updatedTeam
                    in
                    ( State { state | board = board, team = updatedTeam }
                    , Task.succeed (ValidatedFen board updatedTeam rawFen)
                        |> Task.perform config.onFenChanged
                    )

        SetHover position ->
            ( State { state | hover = Just position }, Cmd.none )

        SelectSquare position ->
            if List.member position state.squaresSelected then
                ( State state, Cmd.none )

            else
                ( State { state | squaresSelected = position :: state.squaresSelected }, Cmd.none )


dragConfig : Drag.Config DraggableItem Msg
dragConfig =
    { toMsg = DragMsg
    , finalRelease = FinalRelease

    -- ANIMATION
    , onCursorStartDrag = [ Animation.to present ]
    , onCursorStopDrag = [ Animation.to gone ]
    , onOriginalStartDrag = [ Animation.to gone ]
    , onOriginalStopDrag = [ Animation.to present ]
    }


makeMove : DraggableItem -> Position -> Board -> Board
makeMove from to board =
    board
        |> atPosition Empty from.position
        |> atPosition (Occupied from.player from.piece) to


atPosition : Square -> Position -> Board -> Board
atPosition newSquare target =
    List.indexedMap <|
        \x ->
            List.indexedMap <|
                \y square ->
                    if target == Chess.Data.Position.fromRowColumn x y then
                        newSquare

                    else
                        square


present : List Animation.Property
present =
    [ Animation.opacity 1.0 ]


gone : List Animation.Property
gone =
    [ Animation.opacity 0.0 ]


{-| -}
subscriptions : State -> Sub Msg
subscriptions (State { board, drag }) =
    Drag.subscriptions dragConfig drag



---- VIEW ----


{-| -}
view : Chess.View.Board.Config -> State -> Html Msg
view config (State { board, drag }) =
    Html.span []
        [ Chess.View.Board.grid config (viewCell drag) board
        , viewGhostImage config drag
        ]


viewGhostImage : Chess.View.Board.Config -> Drag.State DraggableItem -> Html Msg
viewGhostImage config drag =
    case drag.subject of
        Nothing ->
            Html.text ""

        Just { player, piece } ->
            Chess.View.Asset.findSvg player
                piece
                (Animation.render drag.cursor
                    ++ [ Html.Attributes.style "max-width" config.each
                       , Html.Attributes.style "max-height" config.each
                       ]
                    ++ followCursor drag.position
                )


followCursor : ( Int, Int ) -> List (Attribute msg)
followCursor ( x, y ) =
    [ Html.Attributes.style "position" "absolute"
    , Html.Attributes.style "top" <| "calc(" ++ String.fromInt y ++ "px - 11em)"
    , Html.Attributes.style "left" <| "calc(" ++ String.fromInt x ++ "px - 9em)"
    , Html.Attributes.style "z-index" "9"
    , Html.Attributes.style "pointer-events" "none"
    ]


viewCell : Drag.State DraggableItem -> Position -> Chess.Data.Board.Square -> Html Msg
viewCell drag position square =
    case square of
        Empty ->
            Chess.View.Board.viewSquare position
                square
                [ onMouseEnter (SetHover position)
                , onClick (SelectSquare position)
                ]
                []

        Occupied player piece ->
            Chess.View.Board.viewSquare position
                square
                (List.concat
                    [ onMouseEnter (SetHover position)
                        :: onClick (SelectSquare position)
                        :: Html.Attributes.style "cursor" "grab"
                        :: Drag.draggableAttributes dragConfig
                            (DraggableItem position player piece)
                    , animateDrag position drag
                    ]
                )
                []


animateDrag : Position -> Drag.State DraggableItem -> List (Attribute Msg)
animateDrag position drag =
    if Maybe.map .position drag.subject == Just position then
        Animation.render drag.original

    else
        []


either : (x -> b) -> (a -> b) -> Result x a -> b
either fromError fromOk result =
    case result of
        Err x ->
            fromError x

        Ok a ->
            fromOk a


nextTeam : Player -> Player
nextTeam player =
    case player of
        White ->
            Black

        Black ->
            White

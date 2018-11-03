module Chess.Data.Board exposing
    ( Board
    , Square(..)
    , boardDecoder
    , toFen
    , blankBoard, boardDecoder2, boardDecoder3, fromFen
    )

{-|

@docs Board
@docs Square
@docs boardDecoder
@docs toFen

-}

import Char
import Chess.Data.Piece exposing (Piece(..))
import Chess.Data.Player as Player
import Dict exposing (Dict)
import Json.Decode as D exposing (..)
import Json.Encode as E


{-| -}
type alias Board =
    List (List Square)



-- {-|
-- ## Holder for board state.
-- -}
-- type Board
--     = Board (List Row)


{-| -- ## Holder for squares
-}
type Row
    = Row (List Square)


{-| -}
type Column
    = Column (List Square)



-- {-| This is how to query a column (usefull for view functions like this one)
-- -- viewSelectedColumn : Square -> List Column -> Column
-- -- viewSelectedColumn square columns =
-- -- List.filter
-- -}
-- fenToColumns : ValidatedFen -> List Column


{-| -}
type Square
    = Empty
    | Occupied Player.Player Piece


{-| -}
boardDecoder : Decoder Board
boardDecoder =
    andThen parseFen string


{-| -}
fromFen : String -> Maybe Board
fromFen fen =
    case D.decodeValue boardDecoder (E.string fen) of
        Err reason ->
            Nothing

        Ok board ->
            Just board


parseFen : String -> Decoder Board
parseFen entireGameState =
    String.words entireGameState
        |> List.head
        |> parseFenHelp


parseFenHelp : Maybe String -> Decoder Board
parseFenHelp entireBoard =
    case entireBoard of
        Just rows ->
            String.split "/" rows
                |> List.foldr (parseFenRow >> map2 (::)) (succeed [])

        Nothing ->
            fail "not a FEN-encoded board"


boardDecoder3 : Decoder Board
boardDecoder3 =
    andThen boardDecoder2 string


boardDecoder2 : String -> Decoder Board
boardDecoder2 entireBoard =
    String.split "/" entireBoard
        |> List.foldr (parseFenRow >> map2 (::)) (succeed [])


parseFenRow : String -> Decoder (List Square)
parseFenRow row =
    String.toList row
        |> List.foldl parseFenRowHelp (succeed [])


parseFenRowHelp : Char -> Decoder (List Square) -> Decoder (List Square)
parseFenRowHelp nextColumn accumulator =
    map2 (++) accumulator (parseFenColumn nextColumn)


parseFenColumn : Char -> Decoder (List Square)
parseFenColumn c =
    if Char.isDigit c then
        succeed <| List.repeat (parseDigit c) Empty

    else
        map List.singleton (pieceDecoder c)


pieceDecoder : Char -> Decoder Square
pieceDecoder pieceKey =
    case Dict.get pieceKey allPieces of
        Just piece ->
            succeed piece

        Nothing ->
            fail "not a FEN-encoded piece"


allPieces : Dict Char Square
allPieces =
    Dict.fromList
        [ ( 'P', Occupied Player.White Pawn )
        , ( 'R', Occupied Player.White Rook )
        , ( 'N', Occupied Player.White Knight )
        , ( 'B', Occupied Player.White Bishop )
        , ( 'Q', Occupied Player.White Hand )
        , ( 'K', Occupied Player.White Monarch )
        , ( 'p', Occupied Player.Black Pawn )
        , ( 'r', Occupied Player.Black Rook )
        , ( 'n', Occupied Player.Black Knight )
        , ( 'b', Occupied Player.Black Bishop )
        , ( 'q', Occupied Player.Black Hand )
        , ( 'k', Occupied Player.Black Monarch )
        ]


parseDigit : Char -> Int
parseDigit c =
    Char.toCode c - 48


blankBoard : Board
blankBoard =
    List.repeat 8 blankRow


blankRow : List Square
blankRow =
    List.repeat 8 Empty



-- TO FEN


{-| -}
toFen : Board -> Player.Player -> String
toFen board team =
    List.map toFenRow board
        |> String.join "/"
        |> withTeam team
        |> withFenSuffix


withTeam : Player.Player -> String -> String
withTeam team partialFen =
    case team of
        Player.White ->
            partialFen ++ " w"

        Player.Black ->
            partialFen ++ " b"


{-| TODO: This does not capture en-passent,
castling, or other niceties of FEN.
-}
withFenSuffix : String -> String
withFenSuffix partialFen =
    partialFen ++ " - - 0 1"


toFenRow : List Square -> String
toFenRow =
    List.foldl toFenRowHelp (Accumulator "" 0) >> collapseEmpties


type alias Accumulator =
    { output : String, empties : Int }


toFenRowHelp : Square -> Accumulator -> Accumulator
toFenRowHelp square accumulator =
    case square of
        Empty ->
            { accumulator | empties = accumulator.empties + 1 }

        Occupied player piece ->
            { empties = 0
            , output =
                collapseEmpties accumulator
                    ++ occupiedString player piece
            }


collapseEmpties : Accumulator -> String
collapseEmpties { output, empties } =
    if empties == 0 then
        output

    else
        output ++ String.fromInt empties


occupiedString : Player.Player -> Piece -> String
occupiedString player piece =
    case ( player, piece ) of
        ( Player.White, Pawn ) ->
            "P"

        ( Player.White, Rook ) ->
            "R"

        ( Player.White, Knight ) ->
            "N"

        ( Player.White, Bishop ) ->
            "B"

        ( Player.White, Hand ) ->
            "Q"

        ( Player.White, Monarch ) ->
            "K"

        ( Player.Black, Pawn ) ->
            "p"

        ( Player.Black, Rook ) ->
            "r"

        ( Player.Black, Knight ) ->
            "n"

        ( Player.Black, Bishop ) ->
            "b"

        ( Player.Black, Hand ) ->
            "q"

        ( Player.Black, Monarch ) ->
            "k"

module Main exposing (..)

import Html exposing (Html, text, div, h1, h2, img, button, br)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Random.Pcg exposing (initialSeed, generate, int, step)


---- MODEL ----
-- Pawns can't be placed in 8th row... ever
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


type alias Placement =
    { square : Int
    , piece : Piece
    , team : Team
    }


type alias Model =
    { currentSeed : Random.Pcg.Seed
    , placements : List Placement
    , points : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { currentSeed = Random.Pcg.initialSeed 12345
      , placements = []
      , points = 1
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Generate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            let
                ( selectedSquare, updatedSeed ) =
                    step (int 1 64) model.currentSeed

                ( pieceNumber, moarUpdatedSeed ) =
                    step (int 1 6) updatedSeed

                ( teamNumber, evenMoarUpdatedSeed ) =
                    step (int 1 2) moarUpdatedSeed

                constructed =
                    { square = selectedSquare
                    , piece = findPieceFromPieceNumber pieceNumber
                    , team = findTeamFromTeamNumber teamNumber
                    }
            in
                ( { model
                    | currentSeed = evenMoarUpdatedSeed
                    , placements = conditionalUpdatedBoard model.points model.placements constructed
                  }
                , Cmd.none
                )


conditionalUpdatedBoard : Int -> List Placement -> Placement -> List Placement
conditionalUpdatedBoard points placements constructed =
    if isIlegal points placements constructed then
        placements
    else
        placements ++ [ constructed ]


isIlegal : Int -> List Placement -> Placement -> Bool
isIlegal points placements constructed =
    squareNotOpen placements constructed
        || monarchAlreadyPlaced placements constructed
        || notEnoughPointsRemaining points placements constructed


squareNotOpen : List Placement -> Placement -> Bool
squareNotOpen placements constructed =
    List.any (\placement -> placement.square == constructed.square) placements


monarchAlreadyPlaced : List Placement -> Placement -> Bool
monarchAlreadyPlaced placements constructed =
    List.any (\placement -> placement.piece == Monarch && constructed.piece == Monarch && placement.team == constructed.team) placements


notEnoughPointsRemaining : Int -> List Placement -> Placement -> Bool
notEnoughPointsRemaining points placements constructed =
    let
        currentTotal =
            List.foldr (\placement total -> total + findPointValueFromPiece placement.piece) 0 placements
    in
        points < currentTotal + findPointValueFromPiece constructed.piece


findPieceFromPieceNumber : Int -> Piece
findPieceFromPieceNumber pieceNumber =
    case pieceNumber of
        1 ->
            Monarch

        2 ->
            Hand

        3 ->
            Rook

        4 ->
            Bishop

        5 ->
            Knight

        6 ->
            Pawn

        _ ->
            Debug.crash "not a valid piece number"


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


findTeamFromTeamNumber : Int -> Team
findTeamFromTeamNumber teamNumber =
    case teamNumber of
        1 ->
            Player

        2 ->
            Opponent

        _ ->
            Debug.crash "not a valid piece number"



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text (toString model.currentSeed) ]
        , h2 [] [ (displayConstructed model.placements) ]
        , button [ onClick Generate ] [ text "Generate pseudo random" ]
        ]


displayConstructed : List Placement -> Html Msg
displayConstructed constructedElements =
    List.foldr (\element result -> result ++ " " ++ (toString element.square) ++ " \x0D\n " ++ (toString element.piece) ++ " " ++ (toString element.team)) "" constructedElements
        |> text



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

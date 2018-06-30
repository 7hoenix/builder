module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Random.Pcg as Random


---- MODEL ----
-- Pawns can't be placed in 8th row... ever
-- Monarch can't be placed in player check
-- Prefer fair games (switch player if better suited).


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
    { currentSeed : Random.Seed
    , placements : List Placement
    , pointsAllowed : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { currentSeed = Random.initialSeed 12345
      , placements = []
      , pointsAllowed = 1
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
                    Random.step (Random.int 1 64) model.currentSeed

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
            ( { model
                | currentSeed = evenMoarUpdatedSeed
                , placements = conditionalUpdatedBoard model.pointsAllowed model.placements constructed
              }
            , Cmd.none
            )


conditionalUpdatedBoard : Int -> List Placement -> Placement -> List Placement
conditionalUpdatedBoard pointsAllowed placements constructed =
    if isIlegal pointsAllowed placements constructed then
        placements
    else
        placements ++ [ constructed ]


isIlegal : Int -> List Placement -> Placement -> Bool
isIlegal pointsAllowed placements constructed =
    squareNotOpen placements constructed
        || monarchAlreadyPlaced placements constructed
        || notEnoughPointsRemaining pointsAllowed placements constructed


squareNotOpen : List Placement -> Placement -> Bool
squareNotOpen placements constructed =
    List.any (\placement -> placement.square == constructed.square) placements


monarchAlreadyPlaced : List Placement -> Placement -> Bool
monarchAlreadyPlaced placements constructed =
    List.any (\placement -> placement.piece == Monarch && constructed.piece == Monarch && placement.team == constructed.team) placements


notEnoughPointsRemaining : Int -> List Placement -> Placement -> Bool
notEnoughPointsRemaining pointsAllowed placements constructed =
    let
        currentTotal =
            List.foldr (\placement total -> total + findPointValueFromPiece placement.piece) 0 placements
    in
    pointsAllowed < currentTotal + findPointValueFromPiece constructed.piece


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
        , button [ onClick Generate ] [ text "Generate pseudo random" ]
        ]


displayConstructed : List Placement -> Html Msg
displayConstructed constructedElements =
    List.foldr (\element result -> result ++ " " ++ toString element.square ++ " \x0D\n " ++ toString element.piece ++ " " ++ toString element.team) "" constructedElements
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

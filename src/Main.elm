module Main exposing (..)

import Html exposing (Html, text, div, h1, h2, img, button, br)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Random.Pcg exposing (initialSeed, generate, int, step)


---- MODEL ----
-- add other pieces
-- add point values
-- point value maximum achieved for each team


type Piece
    = Monarch
    | Hand


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
    }


init : ( Model, Cmd Msg )
init =
    ( { currentSeed = Random.Pcg.initialSeed 12345
      , placements = []
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
                    step (int 1 2) updatedSeed

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
                    , placements = conditionalUpdatedBoard model.placements constructed
                  }
                , Cmd.none
                )


conditionalUpdatedBoard : List Placement -> Placement -> List Placement
conditionalUpdatedBoard placements constructed =
    if isIlegal placements constructed then
        placements
    else
        placements ++ [ constructed ]


isIlegal : List Placement -> Placement -> Bool
isIlegal placements constructed =
    squareOpen placements constructed
        || noMonarchPlaced placements constructed


squareOpen : List Placement -> Placement -> Bool
squareOpen placements constructed =
    List.any (\placement -> placement.square == constructed.square) placements


noMonarchPlaced : List Placement -> Placement -> Bool
noMonarchPlaced placements constructed =
    List.any (\placement -> placement.piece == Monarch && constructed.piece == Monarch && placement.team == constructed.team) placements


findPieceFromPieceNumber : Int -> Piece
findPieceFromPieceNumber pieceNumber =
    case pieceNumber of
        1 ->
            Monarch

        2 ->
            Hand

        _ ->
            Debug.crash "not a valid piece number"


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

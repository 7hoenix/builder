module Main exposing (..)

import Html exposing (Html, text, div, h1, h2, img, button)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Random.Pcg exposing (initialSeed, generate, int, step)


---- MODEL ----


type Piece
    = King
    | Queen


type alias Model =
    { currentSeed : Random.Pcg.Seed
    , selectedSquare : Int
    , selectedPiece : Maybe Piece
    }


init : ( Model, Cmd Msg )
init =
    ( { currentSeed = Random.Pcg.initialSeed 12346
      , selectedSquare = 0
      , selectedPiece = Nothing
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

                ( pieceNumber, evenMoarUpdatedSeed ) =
                    step (int 1 2) updatedSeed
            in
                ( { model
                    | currentSeed = evenMoarUpdatedSeed
                    , selectedSquare = selectedSquare
                    , selectedPiece = findPieceFromPieceNumber pieceNumber
                  }
                , Cmd.none
                )


findPieceFromPieceNumber : Int -> Maybe Piece
findPieceFromPieceNumber pieceNumber =
    case pieceNumber of
        1 ->
            Just King

        2 ->
            Just Queen

        _ ->
            Nothing



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text (toString model.currentSeed) ]
        , h2 [] [ text (toString model.selectedSquare) ]
        , h2 [] [ text (toString model.selectedPiece) ]
        , button [ onClick Generate ] [ text "Generate pseudo random" ]
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

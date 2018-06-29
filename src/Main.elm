module Main exposing (..)

import Html exposing (Html, text, div, h1, h2, img, button)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Random.Pcg exposing (initialSeed, generate, int, step)


---- MODEL ----


type Piece
    = Monarch
    | Hand


type alias PlacedPiece =
    { square : Int
    , piece : Piece
    }


type alias Model =
    { currentSeed : Random.Pcg.Seed
    , board : List PlacedPiece
    }


init : ( Model, Cmd Msg )
init =
    ( { currentSeed = Random.Pcg.initialSeed 12346
      , board = []
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

                constructed =
                    { square = selectedSquare, piece = findPieceFromPieceNumber pieceNumber }
            in
                ( { model
                    | currentSeed = evenMoarUpdatedSeed
                    , board = model.board ++ [ constructed ]
                  }
                , Cmd.none
                )


findPieceFromPieceNumber : Int -> Piece
findPieceFromPieceNumber pieceNumber =
    case pieceNumber of
        1 ->
            Monarch

        2 ->
            Hand

        _ ->
            Debug.crash "not a valid piece number"



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text (toString model.currentSeed) ]
        , h2 [] [ (displayConstructed model.board) ]
        , button [ onClick Generate ] [ text "Generate pseudo random" ]
        ]


displayConstructed : List PlacedPiece -> Html Msg
displayConstructed constructedElements =
    List.foldr (\element result -> result ++ (toString element.square) ++ " " ++ (toString element.piece)) "" constructedElements
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

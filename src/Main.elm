module Main exposing (..)

import Html exposing (Html, text, div, h1, h2, img, button)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Random.Pcg exposing (initialSeed, generate, int, step)


---- MODEL ----


type alias Model =
    { currentSeed : Random.Pcg.Seed
    , generated : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { currentSeed = Random.Pcg.initialSeed 12346, generated = 0 }, Cmd.none )



---- UPDATE ----


type Msg
    = Generate


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            let
                ( generated, updatedSeed ) =
                    step (int 1 100) model.currentSeed
            in
                ( { model
                    | currentSeed = updatedSeed
                    , generated = generated
                  }
                , Cmd.none
                )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text (toString model.currentSeed) ]
        , h2 [] [ text (toString model.generated) ]
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

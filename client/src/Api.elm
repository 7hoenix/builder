module Api exposing (best)

import Chess exposing (ValidatedFen, getRaw, validateFen)
import Http
import Json.Decode exposing (Decoder, andThen, field, string)
import Json.Encode
import Task exposing (Task)



-- ENGINE BACKEND


rawFen : Decoder String
rawFen =
    field "board" string


validatedFen : Decoder ValidatedFen
validatedFen =
    rawFen |> andThen (fromResult << validateFen)


fromResult : Result String a -> Decoder a
fromResult result =
    case result of
        Ok successValue ->
            Json.Decode.succeed successValue

        Err errorMessage ->
            Json.Decode.fail errorMessage


best : String -> ValidatedFen -> Task Http.Error ValidatedFen
best baseEngineUrl currentFen =
    let
        asdf =
            Debug.log "current fen" currentFen
    in
    Http.post
        (baseEngineUrl ++ "/best")
        (Http.jsonBody <|
            Json.Encode.object
                [ ( "board", Json.Encode.string (getRaw currentFen) ) ]
        )
        validatedFen
        |> Http.toTask

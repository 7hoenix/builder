module Api exposing (best)

import Http
import Json.Decode exposing (Decoder, field, string)
import Json.Encode
import Task exposing (Task)



-- ENGINE BACKEND


fen : Decoder String
fen =
    field "board" string


best : String -> String -> Task Http.Error String
best baseEngineUrl state =
    Http.post
        (baseEngineUrl ++ "/best")
        (Http.jsonBody <|
            Json.Encode.object
                [ ( "board", Json.Encode.string state ) ]
        )
        fen
        |> Http.toTask

port module Js exposing (sendMoves)

import Json.Encode as E


port fromElm : E.Value -> Cmd msg


port fromJs : (E.Value -> msg) -> Sub msg


sendMoves : List {} -> Cmd msg
sendMoves moves =
    List.map encodeMove moves
        |> E.list
        |> fromElm


encodeMove : {} -> E.Value
encodeMove move =
    E.null

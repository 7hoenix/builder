port module Js exposing (fromElm, fromJs)

import Json.Encode as E


port fromElm : E.Value -> Cmd msg


port fromJs : (E.Value -> msg) -> Sub msg

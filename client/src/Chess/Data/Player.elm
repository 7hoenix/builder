module Chess.Data.Player exposing
    ( Player(..)
    , player
    )

{-|

@docs Player
@docs player

-}

import Json.Decode exposing (..)


{-| -}
type Player
    = White
    | Black


{-| -}
player : Decoder Player
player =
    andThen parsePlayer string


parsePlayer : String -> Decoder Player
parsePlayer raw =
    case raw of
        "BLACK" ->
            succeed Black

        "WHITE" ->
            succeed White

        invalid ->
            fail <| "`" ++ invalid ++ "` is not a valid player"

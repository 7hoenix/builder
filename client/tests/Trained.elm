module Trained exposing (suite)

import Chess exposing (validateFen)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    describe "The Trained Module"
        [ describe "Chess.validateFen"
            [ test "finds correct fen" <|
                \_ ->
                    Expect.ok <| Chess.validateFen "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
            , fuzz string "errors for everything else" <|
                \randomlyGenerateString ->
                    Expect.err <| Chess.validateFen randomlyGenerateString
            ]
        ]

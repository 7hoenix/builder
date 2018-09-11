module Tests exposing (all)

import Builder exposing (..)
import Chess exposing (Msg, State, fromFen, getSquaresSelected, subscriptions, update, view)
import Dict
import Expect
import Random.Pcg as Random
import Test exposing (..)



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


buildExampleFrame =
    { squares = Hints (Dict.fromList [ ( "a7", "Click this" ), ( "a9", "Mouse over this" ) ])
    , defaultMessage = "Cake"
    }


buildExampleStore =
    Store (Dict.fromList [ ( "8/8/8/8/8/8/8/8 w - -", buildExampleFrame ) ])


buildExampleState =
    let
        blankGameFen =
            "8/8/8/8/8/8/8/8 w - - 0 0"
    in
    case Chess.fromFen blankGameFen of
        Nothing ->
            Debug.crash "FEN PARSER IS BROKEN, PANIC"

        Just state ->
            state


buildExampleModel =
    { apiEndpoint = "some/endpoint"
    , mode = Basic
    , currentFrame = buildExampleFrame
    , store = buildExampleStore
    , initialGameState = Just "8/8/8/8/8/8/8/8 w - - 1 1"
    , initialSeed = Random.initialSeed 1
    , placements = []
    , pointsAllowed = 1
    , submitting = False
    , alerts = []
    , chessModel = buildExampleState
    }


all : Test
all =
    describe "The stuff"
        [ test "handleFenUpdate" <|
            \_ ->
                let
                    updatedModel =
                        buildExampleModel "8/7K/8/8/8/8/8/8 w - - 1 1"
                in
                Expect.equal 10 (2 + 7)

        -- , test "String.left" <| \_ -> Expect.equal "a" (String.left 1 "abcdefg")
        -- , test "This test should fail" <| \_ -> Expect.fail "failed as expected!"
        ]

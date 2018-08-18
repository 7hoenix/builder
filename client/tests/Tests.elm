module Tests exposing (..)

import Builder exposing (Model, Piece(..), Placement, Player(..), init, mapToFen, setFenOnCurrentFrame)
import Expect
import Test exposing (..)


all : Test
all =
    describe "Builder"
        [ test "fromFen works for first one in row" <|
            \_ ->
                let
                    flags =
                        { apiEndpoint = "some/endpoint", initialSeed = 1 }

                    initialModel =
                        init flags |> Tuple.first

                    fenIsh =
                        "P6p/8/8/8/8/8/8/7R more state"

                    modelWithFenSet =
                        setFenOnCurrentFrame initialModel fenIsh

                    actualModel =
                        mapToFen modelWithFenSet

                    expectedPlacements =
                        [ { square = { x = 1, y = 1 }
                          , piece = Pawn
                          , team = White
                          }
                        , { square = { x = 8, y = 1 }
                          , piece = Pawn
                          , team = Black
                          }
                        , { square = { x = 8, y = 8 }
                          , piece = Rook
                          , team = White
                          }
                        ]
                in
                Expect.equal actualModel.placements expectedPlacements
        ]

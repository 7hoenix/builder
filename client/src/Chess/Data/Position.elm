module Chess.Data.Position exposing
    ( Position
    , alongDiagonal
    , fromRowColumn
    , toRowColumn
    , fromString
    )

{-|

@docs Position
@docs alongDiagonal
@docs fromRowColumn
@docs toRowColumn
@docs fromString

-}

import Char
import Dict


{-| -}
type Position
    = Position Int Char


{-| -}
alongDiagonal : Position -> Bool
alongDiagonal (Position row column) =
    modBy 2 row == modBy 2 (Char.toCode column)


{-| -}
fromRowColumn : Int -> Int -> Position
fromRowColumn row column =
    Position
        (row + 1)
        (Char.fromCode (column + 97))


{-| -}
toRowColumn : Position -> ( Int, Int )
toRowColumn (Position row column) =
    ( row - 1, Char.toCode column - 97 )


{-| -}
fromString : String -> Maybe Position
fromString position =
    Dict.get position all


all : Dict.Dict String Position
all =
    Dict.fromList
        [ ( "a8", Position 8 'a' )
        , ( "b8", Position 8 'b' )
        , ( "c8", Position 8 'c' )
        , ( "d8", Position 8 'd' )
        , ( "e8", Position 8 'e' )
        , ( "f8", Position 8 'f' )
        , ( "g8", Position 8 'g' )
        , ( "h8", Position 8 'h' )
        , ( "a7", Position 7 'a' )
        , ( "b7", Position 7 'b' )
        , ( "c7", Position 7 'c' )
        , ( "d7", Position 7 'd' )
        , ( "e7", Position 7 'e' )
        , ( "f7", Position 7 'f' )
        , ( "g7", Position 7 'g' )
        , ( "h7", Position 7 'h' )
        , ( "a6", Position 6 'a' )
        , ( "b6", Position 6 'b' )
        , ( "c6", Position 6 'c' )
        , ( "d6", Position 6 'd' )
        , ( "e6", Position 6 'e' )
        , ( "f6", Position 6 'f' )
        , ( "g6", Position 6 'g' )
        , ( "h6", Position 6 'h' )
        , ( "a5", Position 5 'a' )
        , ( "b5", Position 5 'b' )
        , ( "c5", Position 5 'c' )
        , ( "d5", Position 5 'd' )
        , ( "e5", Position 5 'e' )
        , ( "f5", Position 5 'f' )
        , ( "g5", Position 5 'g' )
        , ( "h5", Position 5 'h' )
        , ( "a4", Position 4 'a' )
        , ( "b4", Position 4 'b' )
        , ( "c4", Position 4 'c' )
        , ( "d4", Position 4 'd' )
        , ( "e4", Position 4 'e' )
        , ( "f4", Position 4 'f' )
        , ( "g4", Position 4 'g' )
        , ( "h4", Position 4 'h' )
        , ( "a3", Position 3 'a' )
        , ( "b3", Position 3 'b' )
        , ( "c3", Position 3 'c' )
        , ( "d3", Position 3 'd' )
        , ( "e3", Position 3 'e' )
        , ( "f3", Position 3 'f' )
        , ( "g3", Position 3 'g' )
        , ( "h3", Position 3 'h' )
        , ( "a2", Position 2 'a' )
        , ( "b2", Position 2 'b' )
        , ( "c2", Position 2 'c' )
        , ( "d2", Position 2 'd' )
        , ( "e2", Position 2 'e' )
        , ( "f2", Position 2 'f' )
        , ( "g2", Position 2 'g' )
        , ( "h2", Position 2 'h' )
        , ( "a1", Position 1 'a' )
        , ( "b1", Position 1 'b' )
        , ( "c1", Position 1 'c' )
        , ( "d1", Position 1 'd' )
        , ( "e1", Position 1 'e' )
        , ( "f1", Position 1 'f' )
        , ( "g1", Position 1 'g' )
        , ( "h1", Position 1 'h' )
        ]

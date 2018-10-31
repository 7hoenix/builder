module Chess.View.Board exposing
    ( Config
    , greenBorder
    , lightGreenBorder
    , grid
    , noBorder
    , redBorder
    , viewSquare
    , yellowBorder
    , blueBorder
    )

{-|

@docs Config
@docs greenBorder
@docs lightGreenBorder
@docs grid
@docs noBorder
@docs redBorder
@docs viewSquare
@docs yellowBorder
@docs blueBorder

-}

import Chess.Data.Board exposing (Square(..))
import Chess.Data.Position exposing (Position)
import Chess.View.Asset
import Html exposing (..)
import Html.Attributes exposing (..)
import Palette


{-| -}
type alias Config =
    { each : String
    , between : String
    , borderSize : String
    }



-- GRID


{-| -}
grid : Config -> (Position -> a -> Html msg) -> List (List a) -> Html msg
grid viewConfig toArea entries =
    let
        axisSize =
            "repeat("
                ++ String.fromInt (List.length entries)
                ++ ","
                ++ viewConfig.each
                ++ ")"
    in
    entries
        |> List.indexedMap (List.indexedMap << viewArea toArea)
        |> List.concat
        |> div
            [ style "display" "grid"
            , style "justify-content" "center"
            , style "grid-gap" viewConfig.between
            , style "grid-template-rows" axisSize
            , style "grid-template-columns" axisSize
            ]


viewArea : (Position -> a -> Html msg) -> Int -> Int -> a -> Html msg
viewArea toArea row column entry =
    let
        position =
            Chess.Data.Position.fromRowColumn row column
    in
    div
        [ style "grid-row" (String.fromInt (row + 1))
        , style "grid-column" (String.fromInt (column + 1))
        ]
        [ toArea position entry ]



-- SQUARES


{-| -}
viewSquare : Position -> Square -> List (Attribute msg) -> List (Attribute msg) -> Html msg
viewSquare position square squareAttributes svgAttributes =
    div
        ([ style "width" "100%"
         , style "height" "100%"
         , style "display" "flex"
         , style "justify-content" "center"
         , style "background-color" (tileBackground position)
         ]
            ++ squareAttributes
        )
        [ case square of
            Empty ->
                text ""

            Occupied player piece ->
                Chess.View.Asset.findSvg player piece <|
                    ([ style "max-height" "100%"
                     , style "max-width" "100%"
                     , style "pointer-events" "none"
                     ]
                        ++ svgAttributes
                    )
        ]


tileBackground : Position -> String
tileBackground position =
    if Chess.Data.Position.alongDiagonal position then
        Palette.purple

    else
        Palette.gray



-- BORDERS


baseBorder : Config -> List (Attribute msg)
baseBorder config =
    [ style "border-width" config.borderSize
    , style "border-style" "solid"
    ]


{-| -}
noBorder : Config -> List (Attribute msg)
noBorder config =
    baseBorder config
        ++ [ style "border-color" "transparent" ]


{-| -}
greenBorder : Config -> List (Attribute msg)
greenBorder config =
    baseBorder config
        ++ [ style "cursor" "pointer"
           , style "border-color" Palette.aqua
           ]


{-| -}
lightGreenBorder : Config -> List (Attribute msg)
lightGreenBorder config =
    baseBorder config
        ++ [ style "cursor" "pointer"
           , style "border-color" Palette.lightGreen
           ]


{-| -}
redBorder : Config -> List (Attribute msg)
redBorder config =
    baseBorder config
        ++ [ style "border-color" Palette.pink ]


{-| -}
yellowBorder : Config -> List (Attribute msg)
yellowBorder config =
    baseBorder config
        ++ [ style "cursor" "pointer"
           , style "border-color" Palette.yellow
           ]


{-| -}
blueBorder : Config -> List (Attribute msg)
blueBorder config =
    baseBorder config
        ++ [ style "cursor" "pointer"
           , style "border-color" Palette.blue
           ]

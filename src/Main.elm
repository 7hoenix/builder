module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)
import Js
import Random.Pcg as Random


---- MODEL ----
-- Monarch can't be placed in player check
-- Prefer fair games (switch player if better suited).


type Piece
    = Monarch
    | Hand
    | Rook
    | Bishop
    | Knight
    | Pawn


type Team
    = Player
    | Opponent


type alias Square =
    { x : Int, y : Int }


type alias Placement =
    { square : Square
    , piece : Piece
    , team : Team
    }


type alias Model =
    { currentSeed : Random.Seed
    , placements : List Placement
    , pointsAllowed : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { currentSeed = Random.initialSeed 12345
      , placements = []
      , pointsAllowed = 6
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Generate
    | GeneratePlacement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Generate ->
            ( generate model, Cmd.none )

        GeneratePlacement ->
            ( generatePlacement model, Cmd.none )


generate : Model -> Model
generate model =
    let
        hasBothMonarchs =
            2 == List.length (List.filter (is Monarch) model.placements)

        atMaxScore =
            currentTotal model.placements == model.pointsAllowed
    in
    if hasBothMonarchs && atMaxScore then
        model
    else
        generate <| generatePlacement model


generatePlacement : Model -> Model
generatePlacement model =
    let
        ( selectedSquare, updatedSeed ) =
            Random.step squareGenerator model.currentSeed

        ( piece, moarUpdatedSeed ) =
            Random.step pieceGenerator updatedSeed

        ( team, evenMoarUpdatedSeed ) =
            Random.step teamGenerator moarUpdatedSeed

        constructed =
            { square = selectedSquare
            , piece = piece
            , team = team
            }
    in
    { model
        | currentSeed = evenMoarUpdatedSeed
        , placements = conditionalUpdatedBoard model.pointsAllowed model.placements constructed
    }


conditionalUpdatedBoard : Int -> List Placement -> Placement -> List Placement
conditionalUpdatedBoard pointsAllowed placements constructed =
    if isLegal pointsAllowed placements constructed then
        placements ++ [ constructed ]
    else
        placements



-- VALIDATION


type alias State =
    { pointsAllowed : Int
    , placements : List Placement
    , constructed : Placement
    }


type Validation
    = Valid
    | Invalid


isLegal : Int -> List Placement -> Placement -> Bool
isLegal pointsAllowed placements constructed =
    let
        state =
            State pointsAllowed placements constructed
    in
    List.all (\validate -> validate state == Valid)
        [ squareNotOpen
        , monarchAlreadyPlaced
        , monarchsNotAdjacent
        , notTooManyPawns
        , pawnsNotInEndRows
        , notEnoughPointsRemaining
        ]


squareNotOpen : State -> Validation
squareNotOpen { placements, constructed } =
    if List.any (\placement -> placement.square == constructed.square) placements then
        Invalid
    else
        Valid


monarchAlreadyPlaced : State -> Validation
monarchAlreadyPlaced { placements, constructed } =
    if List.any (\placement -> placement.piece == Monarch && constructed.piece == Monarch && placement.team == constructed.team) placements then
        Invalid
    else
        Valid


monarchsNotAdjacent : State -> Validation
monarchsNotAdjacent { placements, constructed } =
    let
        findMonarch team =
            List.filter
                (\p -> p.team == team && p.piece == Monarch)
                (constructed :: placements)
    in
    {- (1,2)
       (1,2) . (1,3) . (1,4)
       (2,2) . (2,3) . (2,4) . (2,5)
       (3,2) . (3,3) . (3,4)
    -}
    case ( findMonarch Player, findMonarch Opponent ) of
        ( [ player ], [ opponent ] ) ->
            if
                (abs (player.square.x - opponent.square.x) <= 1)
                    && (abs (player.square.y - opponent.square.y) <= 1)
            then
                Invalid
            else
                Valid

        _ ->
            Valid


notTooManyPawns : State -> Validation
notTooManyPawns { placements, constructed } =
    let
        forConstructed =
            List.filter (is Pawn) (constructed :: placements)
                |> List.filter ((==) constructed.team << .team)
    in
    if List.length forConstructed > 8 then
        Invalid
    else
        Valid


pawnsNotInEndRows : State -> Validation
pawnsNotInEndRows { constructed } =
    if
        (constructed.piece == Pawn)
            && (constructed.square.y == 1 || constructed.square.y == 8)
    then
        Invalid
    else
        Valid


notEnoughPointsRemaining : State -> Validation
notEnoughPointsRemaining { pointsAllowed, placements, constructed } =
    if pointsAllowed < currentTotal placements + findPointValueFromPiece constructed.piece then
        Invalid
    else
        Valid


is : Piece -> Placement -> Bool
is piece placement =
    placement.piece == piece


currentTotal : List Placement -> Int
currentTotal placements =
    List.foldr (\placement total -> total + findPointValueFromPiece placement.piece) 0 placements


findPointValueFromPiece : Piece -> Int
findPointValueFromPiece piece =
    case piece of
        Monarch ->
            0

        Hand ->
            9

        Rook ->
            5

        Bishop ->
            3

        Knight ->
            3

        Pawn ->
            1


squareGenerator : Random.Generator Square
squareGenerator =
    Random.map2 Square (Random.int 1 8) (Random.int 1 8)


pieceGenerator : Random.Generator Piece
pieceGenerator =
    Random.choices <|
        List.map Random.constant
            [ Monarch
            , Hand
            , Rook
            , Bishop
            , Knight
            , Pawn
            ]


teamGenerator : Random.Generator Team
teamGenerator =
    Random.choice Player Opponent



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text (toString model.currentSeed) ]
        , h2 [] [ displayConstructed model.placements ]
        , button [ onClick Generate ] [ text "Generate pseudo random" ]
        ]


displayConstructed : List Placement -> Html Msg
displayConstructed constructedElements =
    List.foldr (\element result -> result ++ " " ++ toString element.square ++ " \x0D\n " ++ toString element.piece ++ " " ++ toString element.team) "" constructedElements
        |> text



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

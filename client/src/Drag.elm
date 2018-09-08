module Drag exposing (Config, Msg, State, draggableAttributes, subscriptions, update)

import Animation
import Animation.Messenger as AM
import Html exposing (Attribute)
import Html.Attributes exposing (draggable)
import Html.Events exposing (onMouseEnter, onWithOptions)
import Json.Decode as D
import Mouse
import Task


type alias State item =
    { subject : Maybe item
    , position : Mouse.Position
    , original : AM.State (Msg item)
    , cursor : AM.State (Msg item)
    }


type Msg item
    = Start item Mouse.Position
    | Stop
    | Reset
    | MouseMove Mouse.Position
    | Animate Animation.Msg


type alias Config item msg =
    { toMsg : Msg item -> msg
    , finalRelease : item -> msg

    -- ANIMATIONS
    , onCursorStartDrag : List (AM.Step (Msg item))
    , onCursorStopDrag : List (AM.Step (Msg item))
    , onOriginalStartDrag : List (AM.Step (Msg item))
    , onOriginalStopDrag : List (AM.Step (Msg item))
    }


update : Config item msg -> Msg item -> State item -> ( State item, Cmd msg )
update config msg state =
    case msg of
        Start subject position ->
            ( { state
                | subject = Just subject
                , position = position
                , original = Animation.interrupt config.onOriginalStartDrag state.original
                , cursor = Animation.interrupt config.onCursorStartDrag state.cursor
              }
            , Cmd.none
            )

        Stop ->
            ( { state
                | cursor = Animation.interrupt config.onCursorStopDrag state.cursor
                , original =
                    Animation.interrupt (config.onOriginalStopDrag ++ [ AM.send Reset ])
                        state.original
              }
            , Maybe.map
                (Task.succeed >> Task.perform config.finalRelease)
                state.subject
                |> Maybe.withDefault Cmd.none
            )

        Reset ->
            ( { state | subject = Nothing }, Cmd.none )

        MouseMove position ->
            ( { state | position = position }, Cmd.none )

        Animate tick ->
            let
                ( cursor, item ) =
                    AM.update tick state.cursor

                ( original, b ) =
                    AM.update tick state.original
            in
            ( { state | original = original, cursor = cursor }
            , Cmd.map config.toMsg <| Cmd.batch [ item, b ]
            )


subscriptions : Config item msg -> State item -> Sub msg
subscriptions { toMsg } state =
    Sub.map toMsg <|
        Sub.batch
            [ Mouse.ups (\_ -> Stop)
            , Animation.subscription Animate [ state.original, state.cursor ]
            , if state.subject == Nothing then
                Sub.none

              else
                Mouse.moves MouseMove
            ]


draggableAttributes : Config item msg -> item -> List (Attribute msg)
draggableAttributes { toMsg } subject =
    [ draggable "true"
    , onDragStart <| toMsg << Start subject
    ]


onDragStart : (Mouse.Position -> msg) -> Attribute msg
onDragStart toMsg =
    onWithOptions "dragstart"
        { preventDefault = True, stopPropagation = True }
        (D.map2 (\x y -> toMsg (Mouse.Position x y))
            (D.field "clientX" D.int)
            (D.field "clientY" D.int)
        )

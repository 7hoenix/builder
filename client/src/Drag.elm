module Drag exposing (Config, Msg, State, draggableAttributes, subscriptions, update)

import Animation
import Animation.Messenger as AM
import Browser.Dom as Dom
import Browser.Events exposing (onMouseMove, onMouseUp)
import Html exposing (Attribute)
import Html.Attributes exposing (draggable)
import Html.Events exposing (custom, onMouseEnter)
import Json.Decode as D
import Task


type alias State item =
    { subject : Maybe item
    , position : ( Int, Int )
    , original : AM.State (Msg item)
    , cursor : AM.State (Msg item)
    }


type Msg item
    = Start item ( Int, Int )
    | Stop
    | Reset
    | MouseMove Int Int
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
        Start subject element ->
            ( { state
                | subject = Just subject
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

        MouseMove x y ->
            ( { state | position = ( x, y ) }, Cmd.none )

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
            [ Animation.subscription Animate [ state.original, state.cursor ]
            , case state.subject of
                Nothing ->
                    Sub.none

                Just _ ->
                    Sub.batch
                        [ onMouseMove (D.map2 MouseMove pageX pageY)
                        , onMouseUp <| D.succeed Stop
                        ]
            ]


draggableAttributes : Config item msg -> item -> List (Attribute msg)
draggableAttributes { toMsg } subject =
    [ draggable "true"
    , onDragStart <| toMsg << Start subject
    ]


onDragStart : (( Int, Int ) -> msg) -> Attribute msg
onDragStart toMsg =
    custom "dragstart" <|
        D.map2
            (\x y ->
                { message = toMsg ( x, y )
                , preventDefault = True
                , stopPropagation = True
                }
            )
            (D.field "clientX" D.int)
            (D.field "clientY" D.int)


pageX : D.Decoder Int
pageX =
    D.field "pageX" D.int


pageY : D.Decoder Int
pageY =
    D.field "pageY" D.int

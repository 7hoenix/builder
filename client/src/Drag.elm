module Drag exposing (Model, Msg, init, subscriptions, update)

-- import Animation
-- import Animation.Messenger as AM

import Browser.Dom as Dom
import Browser.Events exposing (onMouseMove)
import Html exposing (Attribute)
import Html.Attributes exposing (draggable)
import Html.Events exposing (custom, onMouseEnter)
import Json.Decode as D
import Task



-- type alias State item =
--     { subject : Maybe item
--     , elementItsOver : Maybe Dom.Element
--     -- , original : AM.State (Msg item)
--     -- , cursor : AM.State (Msg item)
--     }


type alias Model =
    { x : Int, y : Int, dragState : DragState }


type DragState
    = Static
    | Moving Int Int Int Int


init : () -> ( Model, Cmd (Msg item) )
init _ =
    ( Model 100 100 Static, Cmd.none )



-- type Msg item
--     = Start item Dom.Element
--     | Stop
--     | Reset
--     | MouseMove Dom.Element


type Msg item
    = Start item Int Int
    | Stop Int Int
    | Move Int Int


update : Msg item -> Model -> ( Model, Cmd (Msg item) )
update msg model =
    case msg of
        Start item x y ->
            ( { model | dragState = Moving x y x y }, Cmd.none )

        Move x y ->
            case model.dragState of
                Static ->
                    ( model, Cmd.none )

                Moving startX startY _ _ ->
                    ( { model | dragState = Moving startX startY x y }, Cmd.none )

        Stop x y ->
            case model.dragState of
                Static ->
                    ( model, Cmd.none )

                Moving startX startY _ _ ->
                    ( Model (model.x + startX - x) (model.y + startY - y) Static, Cmd.none )



-- | Animate Animation.Msg
-- type alias Config item msg =
--     { toMsg : Msg item -> msg
--     , finalRelease : item -> msg
--     -- -- ANIMATIONS
--     -- , onCursorStartDrag : List (AM.Step (Msg item))
--     -- , onCursorStopDrag : List (AM.Step (Msg item))
--     -- , onOriginalStartDrag : List (AM.Step (Msg item))
--     -- , onOriginalStopDrag : List (AM.Step (Msg item))
--     }
-- update : Config item msg -> Msg item -> State item -> ( State item, Cmd msg )
-- update config msg state =
--     case msg of
--         Start subject element ->
--             ( { state
--                 | subject = Just subject
--                 , elementItsOver = Just element
--                 -- , original = Animation.interrupt config.onOriginalStartDrag state.original
--                 -- , cursor = Animation.interrupt config.onCursorStartDrag state.cursor
--               }
--             , Cmd.none
--             )
--         Stop ->
--             ( { state | elementItsOver = Nothing }
--             , Maybe.map
--                 (Task.succeed >> Task.perform config.finalRelease)
--                 state.subject
--                 |> Maybe.withDefault Cmd.none
--             )
--         -- ( { state
--         --     | cursor = Animation.interrupt config.onCursorStopDrag state.cursor
--         --     , original =
--         --         Animation.interrupt (config.onOriginalStopDrag ++ [ AM.send Reset ])
--         --             state.original
--         --   }
--         -- , Maybe.map
--         --     (Task.succeed >> Task.perform config.finalRelease)
--         --     state.subject
--         --     |> Maybe.withDefault Cmd.none
--         -- )
--         Reset ->
--             ( { state | subject = Nothing }, Cmd.none )
--         MouseMove element ->
--             ( { state | elementItsOver = Just element }, Cmd.none )
-- Animate tick ->
--     let
--         ( cursor, item ) =
--             AM.update tick state.cursor
--         ( original, b ) =
--             AM.update tick state.original
--     in
--     ( { state | original = original, cursor = cursor }
--     , Cmd.map config.toMsg <| Cmd.batch [ item, b ]
--     )


subscriptions : Model -> Sub (Msg item)
subscriptions model =
    case model.dragState of
        Static ->
            Sub.none

        Moving _ _ _ _ ->
            onMouseMove (D.map2 Move pageX pageY)


pageX : D.Decoder Int
pageX =
    D.field "pageX" D.int


pageY : D.Decoder Int
pageY =
    D.field "pageY" D.int



-- subscriptions : Config item msg -> State item -> Sub msg
-- subscriptions { toMsg } state =
--     Sub.map toMsg <|
--         Sub.batch
--             [ -- Animation.subscription Animate [ state.original, state.cursor ]
--               case state.subject of
--                 Nothing ->
--                     Sub.none
--                 Just _ ->
--                     case state.elementItsOver of
--                         Nothing ->
--                             Sub.none
--                         Just element ->
--                             onMouseMove (D.succeed (MouseMove element))
--             ]
-- { toMsg : Msg item -> msg
-- Sub.batch
-- [
-- ]
-- TODO: do something with this [ Animation.subscription Animate [ state.original, state.cursor ]
-- draggableAttributes : Config item msg -> item -> List (Attribute msg)
-- draggableAttributes { toMsg } subject =
--     []
--     [ draggable "true"
--     , onDragStart <| toMsg << Start subject
--     ]
-- onDragStart : (Dom.Element -> msg) -> Attribute msg
-- onDragStart toMsg =
--     Debug.todo "Add drag"
-- onDragStart : (Dom.Element -> msg) -> Attribute msg
-- onDragStart toMsg =
--     custom "dragstart"
--         { preventDefault = True, stopPropagation = True }
--         (D.map2 (\x y -> toMsg (Browser.Events.onMouseMove x y))
--             (D.field "clientX" D.int)
--             (D.field "clientY" D.int)
--         )

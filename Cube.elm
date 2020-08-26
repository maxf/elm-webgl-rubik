module Cube exposing (main)

{-
   Rotating cube with colored sides.
-}

import Browser
import Browser.Dom exposing (getViewport)
import Browser.Events exposing (onMouseMove, onResize)
import Json.Decode as Decode exposing (Decoder, Value)
import Task

import View exposing (view)
import Types exposing (Move(..), Model, Msg(..))


main : Program Value Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , subscriptions = subscriptions
        , update = update
        }


init : ( Model, Cmd Msg )
init =
    ( { size = { width = 0, height = 0 }
      , position = { x = 0, y = 0 }
      , rotation = { phi = 0, theta = 0 }
      , moves = []
      }
    , Task.perform GetViewport getViewport
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onMouseMove mousePosition
        , onResize Resize
        ]


mousePosition : Decoder Msg
mousePosition =
    Decode.map2 (\x y -> MouseMove { x = x, y = y })
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove position ->
            ( { model | rotation = { phi = position.x / 150, theta = position.y / 150 } }
            , Cmd.none
            )

        Resize width height ->
            ( { model | size = { width = toFloat width, height = toFloat height } }, Cmd.none )

        GetViewport { viewport } ->
            ( { model | size = { width = viewport.width, height = viewport.height } }
            , Cmd.none
            )

        MoveUp ->
            ( model |> addMove Up, Cmd.none )


addMove : Move -> Model -> Model
addMove move model =
    { model | moves = move :: model.moves }

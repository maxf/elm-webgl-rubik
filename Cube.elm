module Cube exposing (main)

{-
   Rotating cube with colored sides.
-}

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events exposing (onMouseMove, onResize)
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Json.Decode as Decode exposing (Decoder, Value)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Task
import WebGL exposing (Mesh, Shader)


type Msg
    = MouseMove { x : Float, y : Float }
    | Resize Int Int
    | GetViewport Viewport



type alias Model =
    { moves : List Int
    , size :
        { width : Float
        , height : Float
        }
    , position :
        { x : Float
        , y : Float
        }
    , rotation :
        { phi : Float
        , theta : Float
        }
    }


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
            ( { model | size = { width = viewport.width, height = viewport.height } }, Cmd.none )


view : Model -> Html Msg
view { size, rotation } =
    WebGL.toHtml
        [ width (round size.width)
        , height (round size.height)
        , style "display" "block"
        , style "position" "absolute"
        , style "left" "0"
        , style "top" "0"
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            cubeMesh
            { perspective =
                perspective size.width size.height
            , rotation =
                Mat4.identity
                    |> Mat4.rotate rotation.phi Vec3.j
                    |> Mat4.rotate rotation.theta Vec3.k
            }
        ]


type alias Uniforms =
    { perspective : Mat4
    , rotation : Mat4
    }


perspective : Float -> Float -> Mat4
perspective width height =
    let
        eye =
            vec3 0.5 -0.5 1
                |> Vec3.normalize
                |> Vec3.scale 6
    in
    Mat4.mul
        (Mat4.makePerspective 45 (width / height) 0.01 100)
        (Mat4.makeLookAt eye (vec3 0 0 0) Vec3.j)



-- Mesh


white : Vec3
white =
    vec3 200 200 200


red : Vec3
red =
    vec3 255 0 0


blue : Vec3
blue =
    vec3 0 0 255


orange : Vec3
orange =
    vec3 245 121 0


green : Vec3
green =
    vec3 0 255 0


yellow : Vec3
yellow =
    vec3 237 212 0


black : Vec3
black =
    vec3 0 0 0


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


cubeMesh : Mesh Vertex
cubeMesh =
    [ cubieMesh [ black, black, black, blue, red, white ] -1 -1 -1 0.9
    , cubieMesh [ black, black, black, black, red, white ] -1 -1 0 0.9
    , cubieMesh [ black, black, green, black, red, white ] -1 -1 1 0.9
    , cubieMesh [ black, black, black, blue, red, black ] -1 0 -1 0.9
    , cubieMesh [ black, black, black, black, red, black ] -1 0 0 0.9
    , cubieMesh [ black, black, green, black, red, black ] -1 0 1 0.9
    , cubieMesh [ black, yellow, black, blue, red, black ] -1 1 -1 0.9
    , cubieMesh [ black, yellow, black, black, red, black ] -1 1 0 0.9
    , cubieMesh [ black, yellow, green, black, red, black ] -1 1 1 0.9
    , cubieMesh [ black, black, black, blue, black, white ] 0 -1 -1 0.9
    , cubieMesh [ black, black, black, black, black, white ] 0 -1 0 0.9
    , cubieMesh [ black, black, green, black, black, white ] 0 -1 1 0.9
    , cubieMesh [ black, black, black, blue, black, black ] 0 0 -1 0.9
    , cubieMesh [ black, black, black, black, black, black ] 0 0 0 0.9
    , cubieMesh [ black, black, green, black, black, black ] 0 0 1 0.9
    , cubieMesh [ black, yellow, black, blue, black, black ] 0 1 -1 0.9
    , cubieMesh [ black, yellow, black, black, black, black ] 0 1 0 0.9
    , cubieMesh [ black, yellow, green, black, black, black ] 0 1 1 0.9
    , cubieMesh [ orange, black, black, blue, black, white ] 1 -1 -1 0.9
    , cubieMesh [ orange, black, black, black, black, white ] 1 -1 0 0.9
    , cubieMesh [ orange, black, green, black, black, white ] 1 -1 1 0.9
    , cubieMesh [ orange, black, black, blue, black, black ] 1 0 -1 0.9
    , cubieMesh [ orange, black, black, black, black, black ] 1 0 0 0.9
    , cubieMesh [ orange, black, green, black, black, black ] 1 0 1 0.9
    , cubieMesh [ orange, yellow, black, blue, black, black ] 1 1 -1 0.9
    , cubieMesh [ orange, yellow, black, black, black, black ] 1 1 0 0.9
    , cubieMesh [ orange, yellow, green, black, black, black ] 1 1 1 0.9
    ]
        |> List.concat
        |> WebGL.triangles


cubieMesh : List Vec3 -> Float -> Float -> Float -> Float -> List ( Vertex, Vertex, Vertex )
cubieMesh colours cx cy cz w =
    let
        rft =
            vec3 (cx + w / 2) (cy + w / 2) (cz + w / 2)

        lft =
            vec3 (cx - w / 2) (cy + w / 2) (cz + w / 2)

        lbt =
            vec3 (cx - w / 2) (cy - w / 2) (cz + w / 2)

        rbt =
            vec3 (cx + w / 2) (cy - w / 2) (cz + w / 2)

        rbb =
            vec3 (cx + w / 2) (cy - w / 2) (cz - w / 2)

        rfb =
            vec3 (cx + w / 2) (cy + w / 2) (cz - w / 2)

        lfb =
            vec3 (cx - w / 2) (cy + w / 2) (cz - w / 2)

        lbb =
            vec3 (cx - w / 2) (cy - w / 2) (cz - w / 2)
    in
    (case colours of
        [ top, bottom, front, back, left, right ] ->
            [ face top rft rfb rbb rbt
            , face bottom rft rfb lfb lft
            , face front rft lft lbt rbt
            , face back rfb lfb lbb rbb
            , face left lft lfb lbb lbt
            , face right rbt rbb lbb lbt
            ]

        _ ->
            [ face green rft rfb rbb rbt
            , face blue rft rfb lfb lft
            , face yellow rft lft lbt rbt
            , face red rfb lfb lbb rbb
            , face white lft lfb lbb lbt
            , face orange rbt rbb lbb lbt
            ]
    )
        |> List.concat


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face color a b c d =
    let
        vertex position =
            Vertex (Vec3.scale (1 / 255) color) position
    in
    [ ( vertex a, vertex b, vertex c )
    , ( vertex c, vertex d, vertex a )
    ]



-- Shaders


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|

        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 rotation;
        uniform mat4 perspective;

        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * rotation * vec4(position, 1.0);
            vcolor = color;
        }

    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|

        precision mediump float;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }

    |]

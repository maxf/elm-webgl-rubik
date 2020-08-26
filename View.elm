module View exposing (view)

import Array exposing (Array)
import Array.Extra
import Html exposing (Html)
import Html.Attributes exposing (height, style, width)
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector3 as Vec3 exposing (Vec3, vec3)
import Html.Events exposing (onClick)
import WebGL exposing (Mesh, Shader)

import Types exposing (Msg(..), Move(..), Model)

type alias Vertex =
    { color : Vec3
    , position : Vec3
    }




type alias Uniforms =
    { perspective : Mat4
    , rotation : Mat4
    }




view : Model -> Html Msg
view { size, rotation, moves } =
    WebGL.toHtml
        [ width (round size.width)
        , height (round size.height)
        , style "display" "block"
        , style "position" "absolute"
        , style "left" "0"
        , style "top" "0"
        , onClick MoveUp
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            (cubeMesh moves)
            { perspective =
                perspective size.width size.height
            , rotation =
                Mat4.identity
                    |> Mat4.rotate rotation.phi Vec3.j
                    |> Mat4.rotate rotation.theta Vec3.k
            }
        ]

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

type alias CubiesPositions 
    = Array (List Float)

rotateFace : Move -> CubiesPositions -> CubiesPositions
rotateFace move pos =
    case move of
        Up ->
            Array.fromList
            [ unsafeGet 0 pos
            , unsafeGet 1 pos
            , unsafeGet 2 pos
            , unsafeGet 3 pos
            , unsafeGet 4 pos
            , unsafeGet 5 pos
            , unsafeGet 8 pos -- 6
            , unsafeGet 17 pos -- 7
            , unsafeGet 26 pos -- 8
            , unsafeGet 9 pos
            , unsafeGet 10 pos
            , unsafeGet 11 pos
            , unsafeGet 12 pos
            , unsafeGet 13 pos
            , unsafeGet 14 pos
            , unsafeGet 7 pos -- 15
            , unsafeGet 16 pos
            , unsafeGet 25 pos -- 17
            , unsafeGet 18 pos
            , unsafeGet 19 pos
            , unsafeGet 20 pos
            , unsafeGet 21 pos
            , unsafeGet 22 pos
            , unsafeGet 23 pos
            , unsafeGet 6 pos -- 24
            , unsafeGet 15 pos -- 25
            , unsafeGet 24 pos -- 26
            ]


cubeMesh : List Move -> Mesh Vertex
cubeMesh moves =
    let 
        config = List.foldl rotateFace initialCubiesPositions moves
    in 
    Array.Extra.map2 
        (\col pos -> cubieMesh col pos) 
        cubiesColours 
        config
        |> Array.toList
        |> List.concat
        |> WebGL.triangles


cubieMesh : List Vec3 -> List Float -> List ( Vertex, Vertex, Vertex )
cubieMesh colours position =
    case position of
        [ cx, cy, cz ] ->
            let
                w = 0.9

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
        _ ->
            face red (vec3 0 0 0) (vec3 1 0 0) (vec3 1 1 0) (vec3 1 1 1)
        


face : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face color a b c d =
    let
        vertex position =
            Vertex (Vec3.scale (1 / 255) color) position
    in
    [ ( vertex a, vertex b, vertex c )
    , ( vertex c, vertex d, vertex a )
    ]



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




cubiesColours : Array (List Vec3)
cubiesColours = 
    [ [  black,  black, black,  blue,   red, white ] 
    , [  black,  black, black, black,   red, white ]
    , [  black,  black, green, black,   red, white ]
    , [  black,  black, black,  blue,   red, black ] 
    , [  black,  black, black, black,   red, black ] 
    , [  black,  black, green, black,   red, black ] 
    , [  black, yellow, black,  blue,   red, black ]
    , [  black, yellow, black, black,   red, black ] 
    , [  black, yellow, green, black,   red, black ] 
    , [  black,  black, black,  blue, black, white ] 
    , [  black,  black, black, black, black, white ] 
    , [  black,  black, green, black, black, white ] 
    , [  black,  black, black,  blue, black, black ] 
    , [  black,  black, black, black, black, black ] 
    , [  black,  black, green, black, black, black ] 
    , [  black, yellow, black,  blue, black, black ] 
    , [  black, yellow, black, black, black, black ] 
    , [  black, yellow, green, black, black, black ] 
    , [ orange,  black, black,  blue, black, white ] 
    , [ orange,  black, black, black, black, white ] 
    , [ orange,  black, green, black, black, white ] 
    , [ orange,  black, black,  blue, black, black ] 
    , [ orange,  black, black, black, black, black ] 
    , [ orange,  black, green, black, black, black ] 
    , [ orange, yellow, black,  blue, black, black ] 
    , [ orange, yellow, black, black, black, black ] 
    , [ orange, yellow, green, black, black, black ]
    ] |> Array.fromList




-- The position in space of each cubie
initialCubiesPositions : CubiesPositions
initialCubiesPositions =
    Array.fromList
        [ [ -1, -1, -1 ] -- 0
        , [ -1, -1,  0 ] -- 1
        , [ -1, -1,  1 ] -- 2
        , [ -1,  0, -1 ] -- 3
        , [ -1,  0,  0 ] -- 4
        , [ -1,  0,  1 ] -- 5
        , [ -1,  1, -1 ] -- 6
        , [ -1,  1,  0 ] -- 7
        , [ -1,  1,  1 ] -- 8
        , [  0, -1, -1 ] -- 9
        , [  0, -1,  0 ] -- 10
        , [  0, -1,  1 ] -- 11
        , [  0,  0, -1 ] -- 12
        , [  0,  0,  0 ] -- 13
        , [  0,  0,  1 ] -- 14
        , [  0,  1, -1 ] -- 15
        , [  0,  1,  0 ] -- 16
        , [  0,  1,  1 ] -- 17
        , [  1, -1, -1 ] -- 18
        , [  1, -1,  0 ] -- 19
        , [  1, -1,  1 ] -- 20
        , [  1,  0, -1 ] -- 21
        , [  1,  0,  0 ] -- 22
        , [  1,  0,  1 ] -- 23
        , [  1,  1, -1 ] -- 24
        , [  1,  1,  0 ] -- 25
        , [  1,  1,  1 ] -- 26
        ]


unsafeGet : Int -> CubiesPositions -> List Float
unsafeGet i pos =
    Array.get i pos |> Maybe.withDefault [ 0, 0, 0 ]

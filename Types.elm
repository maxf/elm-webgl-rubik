module Types exposing (Msg(..), Move(..), Model)

import Browser.Dom exposing (Viewport)


type Msg
    = MouseMove { x : Float, y : Float }
    | Resize Int Int
    | GetViewport Viewport
    | MoveUp

type Move 
    = Up 
    
type alias Model =
    { moves : List Move
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


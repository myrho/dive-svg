module DiveSvg.Model exposing (..)

import Svg exposing (Svg)
import VirtualDom exposing (text)
import Http


type alias Model msg =
    { current : Frame
    , frames : List Frame
    , animation : Maybe Animation
    , slides : Frame -> Svg msg
    }


type Msg
    = Tick Float
    | Forth
    | Back
    | Load (Result Http.Error String)


duration =
    2000


type alias Frame =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }


type alias Animation =
    ( Float, Frame )


init =
    Model (Frame 0 0 0 0) [] Nothing (always (text "Please wait. SVG is loading."))

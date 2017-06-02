module DiveSvg.Model exposing (duration, init, Model, Msg(..), Frame)

{-| .
@docs duration, Model, init, Msg, Frame
-}

import Svg exposing (Svg)
import VirtualDom exposing (text)
import Http


{-| -}
type alias Model msg =
    { current : Frame
    , frames : List Frame
    , animation : Maybe Animation
    , slides : Frame -> Svg msg
    }


{-| -}
type Msg
    = Tick Float
    | Forth
    | Back
    | Load (Result Http.Error String)


{-| -}
duration : Int
duration =
    2000


{-| -}
type alias Frame =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }


type alias Animation =
    ( Float, Frame )


{-| -}
init : Frame -> Model Msg
init current =
    Model current [] Nothing (always (text "Please wait. SVG is loading."))

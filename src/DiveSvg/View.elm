module DiveSvg.View exposing (..)

{-| .
@docs view
-}

import DiveSvg.Model exposing (..)
import Html exposing (Html)


{-| -}
view : Model Msg -> Html Msg
view { slides, current } =
    slides current

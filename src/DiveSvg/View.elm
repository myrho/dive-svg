module DiveSvg.View exposing (..)

{-| .
@docs view
-}

import DiveSvg.Model exposing (..)
import Html exposing (Html)


{-| -}
view : Model -> Html Msg
view { slides, current } =
    slides current

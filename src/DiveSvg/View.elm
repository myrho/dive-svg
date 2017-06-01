module DiveSvg.View exposing (..)

import DiveSvg.Model exposing (..)
import Html exposing (Html)


view : Model Msg -> Html Msg
view { slides, current } =
    slides current

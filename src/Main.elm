module Main exposing (..)

import DiveSvg.Model exposing (Model, Msg)
import DiveSvg.View
import DiveSvg.Sub
import DiveSvg.Update
import Http
import Html


init : { file : String, time : Int } -> ( Model Msg, Cmd Msg )
init { file, time } =
    ( DiveSvg.Model.init
    , Http.getString (file ++ "?" ++ (toString time))
        |> Http.send DiveSvg.Model.Load
    )


main =
    Html.programWithFlags
        { init = init
        , update = DiveSvg.Update.update
        , subscriptions = DiveSvg.Sub.subscriptions
        , view = DiveSvg.View.view
        }

module DiveSvg.Sub exposing (..)

{-| .
@docs subscriptions
-}

import AnimationFrame
import Keyboard
import Mouse
import DiveSvg.Model exposing (..)


{-| -}
subscriptions : Model msg -> Sub Msg
subscriptions model =
    Sub.batch
        [ case model.animation of
            Nothing ->
                Sub.none

            Just _ ->
                AnimationFrame.diffs Tick
        , Mouse.clicks (\_ -> Forth)
        , Keyboard.downs
            (\code ->
                case code of
                    37 ->
                        Back

                    39 ->
                        Forth

                    _ ->
                        Tick 0
            )
        ]

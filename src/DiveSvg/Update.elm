module DiveSvg.Update exposing (update)

{-| .
@docs update
-}

import DiveSvg.Model exposing (..)
import Tuple exposing (..)
import VirtualDom exposing (text)
import Ease
import DiveSvg.Parser exposing (load)


{-| -}
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        calcProgress oldProgress diff =
            oldProgress
                + (diff / toFloat duration)
                |> Basics.min 1

        calcDiff progress current target =
            Frame
                (current.x + (target.x - current.x) * progress)
                (current.y + (target.y - current.y) * progress)
                (current.w + (target.w - current.w) * progress)
                (current.h + (target.h - current.h) * progress)

        targetOrCurrent model =
            Maybe.map second model.animation
                |> Maybe.withDefault model.current
    in
        case msg of
            Load result ->
                case result |> Result.mapError toString |> Result.andThen load of
                    Err err ->
                        ( { model
                            | slides = (\_ -> toString err |> text)
                          }
                        , Cmd.none
                        )

                    Ok model ->
                        ( model, Cmd.none )

            Forth ->
                ( case model.frames of
                    first :: rest ->
                        { model
                            | animation = Just ( 0, first )
                            , frames =
                                rest
                                    ++ [ targetOrCurrent model
                                       ]
                        }

                    _ ->
                        model
                , Cmd.none
                )

            Back ->
                ( case List.reverse model.frames of
                    last :: rest ->
                        { model
                            | animation = Just ( 0, last )
                            , frames = targetOrCurrent model :: List.reverse rest
                        }

                    _ ->
                        model
                , Cmd.none
                )

            Tick diff ->
                case model.animation of
                    Nothing ->
                        ( model, Cmd.none )

                    Just ( progress, target ) ->
                        if progress == 1 then
                            ( { model
                                | animation = Nothing
                              }
                            , Cmd.none
                            )
                        else
                            let
                                newProgress =
                                    calcProgress progress diff

                                easedProgress =
                                    Ease.inOutQuad newProgress

                                current =
                                    calcDiff easedProgress model.current target
                            in
                                ( { model
                                    | current = current
                                    , animation = Just ( newProgress, target )
                                  }
                                , Cmd.none
                                )

module Main exposing (..)

import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame
import Mouse
import Ease


main =
    Platform.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions model =
    Sub.batch
        [ case model.animation of
            Nothing ->
                Sub.none

            Just _ ->
                AnimationFrame.diffs Tick
        , Mouse.clicks (\_ -> Forth)
        ]


type alias Frame =
    { x : Float
    , y : Float
    , w : Float
    , h : Float
    }


init =
    let
        current =
            Frame -9.27 -136.16 348.332 462.596
    in
        ( { current = current
          , animation = Nothing
          , frames =
                [ Frame 151.628 153.509 20.592 15.204
                ]
          }
        , Cmd.none
        )


type Msg
    = Tick Float
    | Forth


duration =
    2000


update msg model =
    let
        calcProgress oldProgress diff =
            oldProgress
                + (diff / duration)
                |> Basics.min 1

        calcDiff progress current target =
            Frame
                (current.x + (target.x - current.x) * (Debug.log "eased" (Ease.inOutQuad (Debug.log "progress" progress))))
                (current.y + (target.y - current.y) * Ease.inOutQuad progress)
                (current.w + (target.w - current.w) * Ease.inOutQuad progress)
                (current.h + (target.h - current.h) * Ease.inOutQuad progress)
    in
        (\model -> ( model, Cmd.none )) <|
            case Debug.log "msg" msg of
                Forth ->
                    { model
                        | animation =
                            List.head model.frames
                                |> Maybe.map ((,) 0)
                        , frames =
                            List.drop 1 model.frames
                    }

                Tick diff ->
                    case model.animation of
                        Nothing ->
                            ( model, Cmd.none )

                        Just ( progress, target ) ->
                            if progress == 1 then
                                { model
                                    | animation = Nothing
                                }
                            else
                                let
                                    newProgress =
                                        calcProgress progress diff

                                    current =
                                        calcDiff newProgress model.current target
                                in
                                    { model
                                        | current = current
                                        , animation = Just ( newProgress, target )
                                    }


current2Viewbox current =
    (toString current.x)
        ++ " "
        ++ (toString current.y)
        ++ " "
        ++ (toString current.w)
        ++ " "
        ++ (toString current.h)


view { current } =
    svg
        [ width "1800"
        , height "1000"
        , current2Viewbox current
            |> viewBox
        ]
        []

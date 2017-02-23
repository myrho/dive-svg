port module Main exposing (..)

import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame
import Mouse
import Ease

main =
  Platform.program
    { init = (init, Cmd.none)
    , update = update
    --, view = view
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
  { current = Frame 0 0 800 600
  , animation = Nothing
  , frames = 
    [ --(100,100, 150,150)
    --, (100,100, 300,300)
    Frame 99.7 112  10 10 
    , Frame 99.7 118  0.5 0.5 
    , Frame 103 118  0.05 0.05 
    , Frame 0 0 800 600 
    , Frame 0 0 1800 1000 
    , Frame 500 500 1800 1000 
    , Frame -1000 -1000 2800 2000 
    ]
  }

type Msg =
   Tick Float
     | Forth

duration =
  2000

update msg model =
  let
    calcProgress oldProgress diff =       
      oldProgress + (diff / duration)
        |> Basics.min 1

    calcDiff progress current target =
      Frame
        ( current.x + (target.x - current.x) * (Debug.log "eased" (Ease.inOutQuad (Debug.log "progress" progress)) ))
        ( current.y + (target.y - current.y) * Ease.inOutQuad progress )
        ( current.w + (target.w - current.w) * Ease.inOutQuad progress )
        ( current.h + (target.h - current.h) * Ease.inOutQuad progress )
  in
  case Debug.log "msg" msg of
    Forth ->
      ( { model
          | animation = 
              List.head model.frames
                |> Maybe.map ((,) 0)
          , frames = 
              List.drop 1 model.frames
        }
      , Cmd.none
      )
    Tick diff ->
      case model.animation of
        Nothing ->
          (model, Cmd.none)
        Just (progress, target) ->
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
              current = 
                calcDiff newProgress model.current target
            in
              ( { model
                  | current = current
                  , animation = Just (newProgress, target)
                }
              , current2Viewbox current
                |> viewbox
              )

current2Viewbox current =
  (toString current.x) ++ " "
      ++ (toString current.y) ++ " "
      ++ (toString current.w) ++ " "
      ++ (toString current.h)

view {current} =
  svg
    [ width "1800"
    , height "1000"
    , current2Viewbox current
      |> viewBox
    ]
    []


port viewbox : String -> Cmd msg


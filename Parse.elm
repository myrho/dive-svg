module Parse exposing (..)

import Xml exposing (..)
import Xml.Decode exposing (decode)
import VirtualDom exposing (attribute, text)
import Dict
import Json.Encode as Enc
import String
import Svg exposing (svg, node, rect, Svg)
import Svg.Attributes exposing (x, y, width, height, viewBox)
import Tuple exposing (..)
import Html
import Regex exposing (HowMany(AtMost))
import AnimationFrame
import Keyboard
import Mouse
import Ease
import Matrix3 exposing (Float3x3)
import Maybe.Extra
import Dict exposing (Dict)
import Http


init =
    Model (Frame 0 0 0 0) [] Nothing (always (text "no svg loaded"))


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
                case Debug.log "code" code of
                    37 ->
                        Back

                    39 ->
                        Forth

                    _ ->
                        Tick 0
            )
        ]


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

        targetOrCurrent model =
            Maybe.map second model.animation
                |> Maybe.withDefault model.current
    in
        case Debug.log "msg" msg of
            Load result ->
                case result of
                    Err err ->
                        ( { model
                            | slides = (\_ -> toString err |> text)
                          }
                        , Cmd.none
                        )

                    Ok xml ->
                        let
                            ( slides, frames ) =
                                decode xml
                                    |> Result.map parseRoot
                                    |> (\result ->
                                            case result of
                                                Ok ( slides, frames ) ->
                                                    ( slides
                                                    , Debug.log "Frames" frames
                                                    )

                                                Err err ->
                                                    ( (\_ -> text err)
                                                    , []
                                                    )
                                       )

                            model =
                                case frames of
                                    head :: rest ->
                                        Model head rest Nothing slides

                                    _ ->
                                        init
                        in
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

                                current =
                                    calcDiff newProgress model.current target
                            in
                                ( { model
                                    | current = current
                                    , animation = Just ( newProgress, target )
                                  }
                                , Cmd.none
                                )


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


type alias Model msg =
    { current : Frame
    , frames : List Frame
    , animation : Maybe Animation
    , slides : Frame -> Svg msg
    }


type alias Animation =
    ( Float, Frame )


getFloat key attr =
    case Debug.log "getFloat" <| Dict.get key attr of
        Just (FloatNode v) ->
            Just v

        Just (IntNode v) ->
            Just <| toFloat v

        _ ->
            Nothing


findNumber children =
    case Debug.log "findNumber" children of
        IntNode nr ->
            Just nr

        Tag _ _ children ->
            findNumber children

        Object (first :: _) ->
            findNumber first

        _ ->
            Nothing


op2Matrix op =
    Maybe.withDefault Matrix3.identity <|
        if String.startsWith "translate" op then
            numbers2 op
                |> Maybe.map
                    (\( x, y ) ->
                        ( ( 1, 0, 0 )
                        , ( 0, 1, 0 )
                        , ( x, y, 1 )
                        )
                    )
        else if String.startsWith "rotate" op then
            Nothing
        else if String.startsWith "scale" op then
            Nothing
        else if String.startsWith "skewX" op then
            Nothing
        else if String.startsWith "skewY" op then
            Nothing
        else if String.startsWith "matrix" op then
            numbers6 op
                |> Maybe.map
                    (\( a, b, c, d, e, f ) ->
                        ( ( a, b, 0 )
                        , ( c, d, 0 )
                        , ( e, f, 1 )
                        )
                    )
        else
            Nothing


floatRegex =
    Regex.regex "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?"


numbers2 op =
    case Regex.find (AtMost 2) floatRegex op |> Debug.log "floatRegex" of
        { match } :: rest ->
            String.toFloat match
                |> Result.toMaybe
                |> Maybe.map
                    (\x ->
                        List.head rest
                            |> Maybe.map (.match >> String.toFloat >> Result.toMaybe)
                            |> Maybe.Extra.join
                            |> Maybe.map ((,) x)
                    )
                |> Maybe.Extra.join

        _ ->
            Nothing


numbers6 op =
    let
        toMaybe =
            .match >> String.toFloat >> Result.toMaybe
    in
        case Regex.find (AtMost 6) floatRegex op |> Debug.log "floatRegex" of
            a :: b :: c :: d :: e :: f :: _ ->
                map6 (,,,,,)
                    (toMaybe a)
                    (toMaybe b)
                    (toMaybe c)
                    (toMaybe d)
                    (toMaybe e)
                    (toMaybe f)

            _ ->
                Nothing


map6 mapFunc a b c d e f =
    Maybe.map2 (\( a, b, c ) ( d, e, f ) -> mapFunc a b c d e f)
        (Maybe.map3 (,,) a b c)
        (Maybe.map3 (,,) d e f)


frame2Matrix { x, y, w, h } =
    ( ( w, 0, 0 )
    , ( 0, h, 0 )
    , ( x, y, 1 )
    )


matrix2Frame ( ( w, _, _ ), ( _, h, _ ), ( x, y, _ ) ) =
    Frame x y w h


attr2Matrix : Dict String Value -> Float3x3
attr2Matrix attr =
    case Dict.get "transform" attr of
        Just (StrNode value) ->
            Regex.find Regex.All (Regex.regex "(translate|scale|matrix|skewX|skewY|rotate)\\(.+?\\)") value
                |> List.foldr
                    (\{ match } matrix ->
                        op2Matrix match
                            |> Matrix3.mul matrix
                    )
                    Matrix3.identity

        _ ->
            Matrix3.identity


transformFrame : Float3x3 -> Frame -> Frame
transformFrame matrix frame =
    matrix
        |> Matrix3.mul (frame2Matrix frame |> Debug.log "oldFrame")
        |> matrix2Frame
        |> Debug.log "newframe"


foldFrame child ( nr, frame ) =
    case child of
        Tag name attr children ->
            if name == "rect" then
                let
                    _ =
                        Debug.log "rect found" attr
                in
                    case Dict.get "style" attr of
                        Just (StrNode style) ->
                            if not <| String.contains "stroke:#ff0000" style then
                                ( nr, frame )
                            else
                                ( nr
                                , getFloat "x" attr
                                    |> Maybe.andThen
                                        (\x ->
                                            getFloat "y" attr
                                                |> Maybe.andThen
                                                    (\y ->
                                                        getFloat "width" attr
                                                            |> Maybe.andThen
                                                                (\width ->
                                                                    getFloat "height" attr
                                                                        |> Maybe.andThen
                                                                            (\height ->
                                                                                Frame x y width height
                                                                                    |> Just
                                                                            )
                                                                )
                                                    )
                                        )
                                )

                        _ ->
                            ( nr, frame )
            else if name == "text" then
                ( case findNumber children of
                    Just nr ->
                        Just nr

                    Nothing ->
                        nr
                , frame
                )
            else
                ( nr, frame )

        _ ->
            ( nr, frame )


findFrame name attr children =
    if name /= "g" then
        ( Nothing, Nothing )
    else
        case children of
            Object children ->
                List.foldl foldFrame ( Nothing, Nothing ) children

            _ ->
                ( Nothing, Nothing )


frame2Viewbox current =
    (toString current.x)
        ++ " "
        ++ (toString current.y)
        ++ " "
        ++ (toString current.w)
        ++ " "
        ++ (toString current.h)


parseRoot : Xml.Value -> ( Frame -> Svg msg, List Frame )
parseRoot value =
    let
        errorMsg msg =
            ( \_ -> text msg
            , []
            )

        isSvgTag item =
            case item of
                Tag "svg" _ _ ->
                    True

                _ ->
                    False
    in
        case value of
            Object roots ->
                case List.filter isSvgTag roots |> Debug.log "filtered" of
                    (Tag "svg" attr children) :: _ ->
                        let
                            ( nodes, frames ) =
                                parse [] (attr2Matrix attr) children
                        in
                            ( (\frame ->
                                svg
                                    ((toAttr attr)
                                        ++ [ frame2Viewbox frame
                                                |> viewBox
                                           , width "99vw"
                                           , height "99vh"
                                           ]
                                    )
                                    nodes
                              )
                            , List.sortBy first frames
                                |> List.map second
                            )

                    _ ->
                        errorMsg "no svg tag found"

            _ ->
                errorMsg "no svg tag found"


parse : List ( Int, Frame ) -> Float3x3 -> Xml.Value -> ( List (VirtualDom.Node msg), List ( Int, Frame ) )
parse frames parentTransformations value =
    case Debug.log "value" value of
        Tag name attr children ->
            let
                parentTransformations_ =
                    parentTransformations
                        |> Matrix3.mul (attr2Matrix attr)
            in
                case findFrame name attr children of
                    ( Just nr, Just frame ) ->
                        ( [], ( nr, transformFrame parentTransformations_ frame ) :: frames )

                    _ ->
                        let
                            ( nodes, frames_ ) =
                                parse frames parentTransformations_ children
                        in
                            ( [ Svg.node name
                                    (toAttr attr)
                                    nodes
                              ]
                            , frames_
                            )

        Object children ->
            List.map (parse frames parentTransformations) children
                |> List.unzip
                |> mapFirst List.concat
                |> mapSecond List.concat

        StrNode str ->
            ( [ text str
              ]
            , frames
            )

        IntNode int ->
            ( [ toString int
                    |> text
              ]
            , frames
            )

        FloatNode fl ->
            ( [ toString fl
                    |> text
              ]
            , frames
            )

        BoolNode b ->
            ( [ toString b
                    |> text
              ]
            , frames
            )

        DocType _ _ ->
            ( [], frames )


toAttr =
    Dict.toList
        >> List.map
            (\( key, value ) ->
                attribute key <|
                    case value of
                        StrNode str ->
                            str

                        IntNode i ->
                            toString i

                        FloatNode f ->
                            toString f

                        BoolNode b ->
                            toString b
                                |> String.toLower

                        _ ->
                            ""
            )


main =
    Html.programWithFlags
        { init = \t -> ( init, Http.getString ("presentation.svg?" ++ (toString (t / 1))) |> Http.send Load )
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


view { slides, current } =
    slides current

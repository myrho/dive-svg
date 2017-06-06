module DiveSvg.Parser exposing (load)

{-| .
@docs load
-}

import Xml.Parser exposing (..)
import DiveSvg.Model exposing (..)
import Dict exposing (Dict)
import Matrix3 exposing (Float3x3)
import Regex exposing (HowMany(AtMost))
import Svg exposing (svg, node, rect, Svg, tspan)
import Svg.Attributes exposing (x, y, width, height, viewBox)
import VirtualDom exposing (attribute, property, text)
import Tuple exposing (..)
import Json.Encode as Enc
import String


getFloat key attr =
    Dict.fromList attr
        |> Dict.get key
        |> Maybe.andThen (String.toFloat >> Result.toMaybe)


findNumber child result =
    case result of
        Nothing ->
            case child of
                Body nr ->
                    String.toFloat nr
                        |> Result.toMaybe

                Element _ _ children ->
                    List.foldl findNumber Nothing children

                _ ->
                    Nothing

        Just nr ->
            Just nr


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
    case Regex.find (AtMost 2) floatRegex op of
        { match } :: rest ->
            String.toFloat match
                |> Result.toMaybe
                |> Maybe.andThen
                    (\x ->
                        List.head rest
                            |> Maybe.map (.match >> String.toFloat)
                            |> Maybe.andThen Result.toMaybe
                            |> Maybe.map ((,) x)
                    )

        _ ->
            Nothing


numbers6 op =
    let
        toMaybe =
            .match >> String.toFloat >> Result.toMaybe
    in
        case Regex.find (AtMost 6) floatRegex op of
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


attr2Matrix attr =
    case Dict.fromList attr |> Dict.get "transform" of
        Just value ->
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
        |> Matrix3.mul (frame2Matrix frame)
        |> matrix2Frame


foldFrame child ( nr, frame ) =
    let
        getFrame attr =
            getFloat "x" attr
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
    in
        case child of
            Element name attr children ->
                if name == "rect" then
                    let
                        dict =
                            Dict.fromList attr
                    in
                        case Dict.get "style" dict of
                            Just style ->
                                if not <| String.contains "stroke:#ff0000" style then
                                    ( nr, frame )
                                else
                                    ( nr
                                    , getFrame attr
                                    )

                            _ ->
                                case Dict.get "stroke" dict of
                                    Just color ->
                                        if color /= "#ff0000" then
                                            ( nr, frame )
                                        else
                                            ( nr
                                            , getFrame attr
                                            )

                                    _ ->
                                        ( nr, frame )
                else if name == "text" then
                    ( case List.foldl findNumber Nothing children of
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
        List.foldl foldFrame ( Nothing, Nothing ) children


frame2Viewbox current =
    (toString current.x)
        ++ " "
        ++ (toString current.y)
        ++ " "
        ++ (toString current.w)
        ++ " "
        ++ (toString current.h)


parseRoot : List XmlAst -> ( Frame -> Svg msg, List Frame )
parseRoot roots =
    let
        errorMsg msg =
            ( \_ -> text msg
            , []
            )

        isSvgTag item =
            case item of
                Element "svg" _ _ ->
                    True

                _ ->
                    False
    in
        case List.filter isSvgTag roots of
            (Element "svg" attr children) :: _ ->
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
                                   , attribute "preserveAspectRatio" "xMidYMid meet"
                                   ]
                            )
                            nodes
                      )
                    , List.sortBy first frames
                        |> List.map second
                    )

            _ ->
                errorMsg "no svg tag found"


parse : List ( Float, Frame ) -> Float3x3 -> List XmlAst -> ( List (VirtualDom.Node msg), List ( Float, Frame ) )
parse frames parentTransformations children =
    List.map (parseChild frames parentTransformations) children
        |> List.unzip
        |> mapFirst List.concat
        |> mapSecond List.concat


parseChild : List ( Float, Frame ) -> Float3x3 -> XmlAst -> ( List (VirtualDom.Node msg), List ( Float, Frame ) )
parseChild frames parentTransformations child =
    case child of
        Element "image" attr children ->
            ( [ Svg.node "image"
                    (List.map
                        (\( k, v ) ->
                            if k == "xlink:href" then
                                ( "href", v )
                            else
                                ( k, v )
                        )
                        attr
                        |> toAttr
                    )
                    []
              ]
            , frames
            )

        Element name attr children ->
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

        Body str ->
            ( [ tspan
                    [ Enc.string str
                        |> property "innerHTML"
                    ]
                    []
              ]
            , frames
            )

        _ ->
            ( [], frames )


toAttr =
    List.map (uncurry attribute)


{-| -}
load : String -> Result String Model
load xml =
    let
        ( slides, frames ) =
            parseXml xml
                |> Result.map parseRoot
                |> (\result ->
                        case result of
                            Ok ( slides, frames ) ->
                                ( slides
                                , frames
                                )

                            Err err ->
                                ( (\_ -> text <| toString err)
                                , []
                                )
                   )
    in
        case frames of
            head :: rest ->
                Ok <| Model head rest Nothing slides

            _ ->
                Err "no frames found"

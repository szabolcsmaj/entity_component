module View exposing (view)

import Html exposing (..)
import Message exposing (..)
import Model exposing (..)
import Regex exposing (..)
import List exposing (..)
import String exposing (..)
import Debug exposing (..)


view : RootNode -> Html Msg
view rootNode =
    div []
        [ h1 [] [ text "Node data:" ]
        , br [] []
        , div []
            [ h3 [] [ text ("Class: " ++ rootNode.class) ]
            , h3 [] [ text ("Method: " ++ rootNode.method) ]
            ]
        , br [] []
        , div []
            [ ul []
                [ displayNodes rootNode.variables
                ]
            ]
        ]


displayNodes : List Node -> Html Msg
displayNodes nodes =
    let
        ( _, vars ) =
            loopVariables nodes []
    in
        div [] vars


loopVariables : List Node -> List (Html Msg) -> ( List Node, List (Html Msg) )
loopVariables variables previous =
    let
        createNewNodeHtml variable =
            li []
                [ text variable.name
                , text ("(" ++ variable.nodeType ++ "): ")
                , displayNodeValue variable.value
                ]
    in
        case variables of
            [] ->
                ( [], previous )

            [ variable ] ->
                ( [], previous ++ [ createNewNodeHtml variable ] )

            variable :: remaining ->
                loopVariables remaining (previous ++ [ createNewNodeHtml variable ])


displayNodeValue : String -> Html Msg
displayNodeValue value =
    if isObject value then
        div []
            [ text (toString value)
            , convertNodeValueToObject value
            ]
    else
        div [] [ text value ]


objectRegex : String
objectRegex =
    "^(.+)(@[0-9a-e]{8})?\\[(.+)\\]$"


isObject : String -> Bool
isObject value =
    Regex.contains (regex objectRegex) value


convertNodeValueToObject : String -> Html Msg
convertNodeValueToObject value =
    let
        matches =
            find All (regex objectRegex) value
                |> List.map .submatches

        maybeValues =
            head matches `Maybe.andThen` head `Maybe.andThen` (\x -> x)
    in
        case maybeValues of
            Just objectValue ->
                ul []
                    [ doConvertObject objectValue
                    ]

            Nothing ->
                text value


doConvertObject : String -> Html Msg
doConvertObject value =
    let
        inflateObject =
            value
                |> String.split ","
                |> List.map (\x -> (String.split "=" x))
                |> List.filterMap createNode

        ( _, vars ) =
            loopVariables inflateObject []
    in
        li [] vars


createNode : List String -> Maybe Node
createNode list =
    let
        keyValues =
            list
                |> List.filter (\x -> x /= "=")
    in
        case keyValues of
            key :: rest ->
                case rest of
                    [] ->
                        Nothing

                    [ value ] ->
                        Just (Node key "unknown" value)

                    possibleObjectSliced ->
                        let
                            possibleObject =
                                String.join "," possibleObjectSliced
                        in
                            if isObject possibleObject then
                                createNode [ possibleObject ]
                            else
                                Nothing

            [] ->
                Nothing

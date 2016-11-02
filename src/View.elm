module View exposing (view)

import Html exposing (..)
import Message exposing (..)
import Model exposing (..)
import Regex exposing (..)
import List exposing (..)
import String exposing (..)
import Formatters exposing (determineType)


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
            loopNodes nodes []
    in
        div [] vars


loopNodes : List Node -> List (Html Msg) -> ( List Node, List (Html Msg) )
loopNodes nodes elements =
    let
        createNodeHtml node =
            li []
                [ text node.name
                , text ("(" ++ node.nodeType ++ "): ")
                , displayNodeValue node.value
                ]
    in
        case nodes of
            [] ->
                ( [], elements )

            [ node ] ->
                ( [], elements ++ [ createNodeHtml node ] )

            node :: remaining ->
                loopNodes remaining (elements ++ [ createNodeHtml node ])


displayNodeValue : String -> Html Msg
displayNodeValue value =
    if isObject value then
        div []
            [ text (toString value)
              -- TODO: loopNodes should be called here
            , convertNodeValueToObject value
            ]
    else
        div [] [ text value ]


objectRegex : String
objectRegex =
    "^([a-zA-Z0-9.]+)(@[0-9a-e]{8})?\\[(.+)\\]$"


isObject : String -> Bool
isObject value =
    Regex.contains (regex objectRegex) value


convertNodeValueToObject : String -> Html Msg
convertNodeValueToObject value =
    let
        matches =
            find All (regex objectRegex) value
                |> List.map .submatches
                |> List.head

        reverse_matches =
            case matches of
                Just match_list ->
                    List.reverse match_list

                Nothing ->
                    []

        maybeValues =
            head reverse_matches `Maybe.andThen` (\x -> x)
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
            loopNodes inflateObject []
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
                        Just (Node key (determineType value) value)

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

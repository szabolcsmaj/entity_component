module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
        variables =
            loopNodes nodes []
    in
        div [] variables


loopNodes : List Node -> List (Html Msg) -> List (Html Msg)
loopNodes nodes elements =
    let
        createNodeHtml node =
            li []
                [ text node.name
                  --, text ("(" ++ (toString node.id) ++ ")")
                , text ("(" ++ node.nodeType ++ "): ")
                , displayNodeValue node
                ]
    in
        case nodes of
            [] ->
                elements

            [ node ] ->
                elements ++ [ createNodeHtml node ]

            node :: remaining ->
                loopNodes remaining (elements ++ [ createNodeHtml node ])


nodeStyle : Bool -> List ( String, String )
nodeStyle extended =
    let
        attributeName =
            "display"
    in
        if extended then
            []
        else
            [ ( attributeName, "none" ) ]


extractNodeValues : PossibleNodes -> Maybe (List Node)
extractNodeValues (PossibleNodes nodeValues) =
    nodeValues


displayNodeValue : Node -> Html Msg
displayNodeValue node =
    let
        getStringValue =
            case node.value.stringValue of
                Just stringValue ->
                    stringValue

                Nothing ->
                    ""

        displayChildNodes node =
            case (extractNodeValues node.value.nodeValues) of
                Just values ->
                    ul []
                        [ displayNodes values ]

                Nothing ->
                    div [] []
    in
        if node.value.isObject then
            if node.extended then
                div [ style [ ( "display", "inline" ) ] ]
                    [ button [ onClick (SwitchExtended node.id) ] [ text "-" ]
                    , displayChildNodes node
                    ]
            else
                button [ onClick (SwitchExtended node.id) ] [ text "+" ]
        else
            case node.value.stringValue of
                Just stringValue ->
                    text stringValue

                Nothing ->
                    text ""

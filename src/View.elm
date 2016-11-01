module View exposing (..)

import Html exposing (..)
import Message exposing (..)
import Model exposing (..)


view : RootNode -> Html Msg
view node =
    div []
        [ h1 [] [ text "Node data:"]
        , br [] []
        , div [] [
            h3 [] [ text ("Class: " ++ node.class) ]
            , h3 [] [ text ("Method: " ++ node.method) ]
            ]
        , br [][]
        , div [] [
            expandNode node
            ]
        ]

expandNode : RootNode -> Html Msg
expandNode node =
    let
        (_, vars) = loopVariables node.variables []
    in 
        div [] vars

loopVariables : List Node -> List (Html Msg) -> (List Node, List (Html Msg))
loopVariables variables previous =
    let 
        createNewLine variable =
            div [] [ 
                br [][],
                text variable.name, 
                text ("(" ++ variable.nodeType ++ "): ") ,
                showValue variable.value
                ]
    in
        case variables of
            [] -> ([], previous)
            [variable] -> ([], previous ++ [createNewLine variable])
            (variable::remaining) -> loopVariables remaining (previous ++ [createNewLine variable])

showValue : String -> Html Msg
showValue value =
    text value

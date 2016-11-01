module View exposing (..)

import Html exposing (..)
import Message exposing (..)
import Model exposing (..)
import Regex exposing (..)
import List exposing (..)


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
            ul [] [
            expandNode node
            ]
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
            li [] [
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
    if isObject value then
       div [] [ convertToObject value ]
    else
       div [] [ text value ]

objectString : String
objectString =
    "^.+@[0-9a-e]{8}\\[(.+)\\]$"

isObject : String -> Bool
isObject value =
    contains (regex "^.+@[0-9a-e]{8}\\[.+\\]$") value

convertToObject : String -> Html Msg
convertToObject value =
    let 
        matches = 
            find All (regex objectString) value
            |> List.map .submatches

        maybeValues =
            head matches `Maybe.andThen` head `Maybe.andThen` (\x -> x)
    in 
       case maybeValues of
           Just objectValue ->
               doConvertObject objectValue
           Nothing ->
               text value



doConvertObject : String -> Html Msg
doConvertObject value =
    text value

module View exposing (view, getObjectWithClosingBracket, splitValue)

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


displayNodeValue : Node -> Html Msg
displayNodeValue node =
    let
        getStringValue =
            case node.value.stringValue of
                Just stringValue ->
                    stringValue

                Nothing ->
                    ""
    in
        if isObject getStringValue then
            if node.extended then
                div [ style [ ( "display", "inline" ) ] ] [ button [ onClick (SwitchExtended node.name) ] [ text "-" ], convertNodeValueToObject getStringValue ]
            else
                button [ onClick (SwitchExtended node.name) ] [ text "+" ]
        else
            case node.value.stringValue of
                Just stringValue ->
                    text stringValue

                Nothing ->
                    text ""


objectRegex : String
objectRegex =
    -- com.company.ObjectName@1234abcd[id=1,name="QQQ"]
    "^(([a-zA-Z0-9.]+)(@[0-9a-f]{8})?)\\[(.+)\\]$"


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
                ul [] (doConvertObject objectValue)

            Nothing ->
                text value


doConvertObject : String -> List (Html Msg)
doConvertObject value =
    let
        inflateObject =
            value
                |> splitValue
                |> List.filterMap createNode
    in
        loopNodes inflateObject []


splitValue : String -> List String
splitValue value =
    let
        valueList =
            doSplitValue value []
    in
        valueList |> List.reverse


doSplitValue : String -> List String -> List String
doSplitValue remainingPart values =
    let
        splitted =
            Regex.split (AtMost 1) (regex ",") remainingPart
    in
        case splitted of
            [] ->
                values

            [ value ] ->
                value :: values

            value :: remainder ->
                let
                    objectBeginningPattern =
                        "^(.+)=[a-zA-Z0-9._@]+\\[(.*)$"

                    isAnotherObjectBeginning =
                        Regex.contains (regex objectBeginningPattern) value

                    remainderString =
                        String.join "," remainder

                    ( newValue, newRemainder ) =
                        if isAnotherObjectBeginning then
                            -- We rejoin the splitted string
                            getObjectWithClosingBracket (value ++ "," ++ remainderString)
                        else
                            ( value, remainderString )
                in
                    doSplitValue newRemainder (newValue :: values)


getObjectWithClosingBracket : String -> ( String, String )
getObjectWithClosingBracket text =
    doGetObjectWithClosingBracket text "" 0


doGetObjectWithClosingBracket : String -> String -> Int -> ( String, String )
doGetObjectWithClosingBracket remainingText objectText bracketCounter =
    case (uncons remainingText) of
        Just ( head, tail ) ->
            let
                newObjectText =
                    objectText ++ (String.fromChar head)
            in
                case head of
                    ']' ->
                        if (bracketCounter == 1) then
                            let
                                finalRemainder =
                                    case (uncons tail) of
                                        Just ( ',', tailOfTail ) ->
                                            tailOfTail

                                        _ ->
                                            tail
                            in
                                ( newObjectText, finalRemainder )
                        else
                            doGetObjectWithClosingBracket tail newObjectText (bracketCounter - 1)

                    '[' ->
                        doGetObjectWithClosingBracket tail newObjectText (bracketCounter + 1)

                    _ ->
                        doGetObjectWithClosingBracket tail newObjectText bracketCounter

        Nothing ->
            ( remainingText, remainingText )


createNode : String -> Maybe Node
createNode keyValue =
    let
        splitKeyValue =
            Regex.split (AtMost 1) (regex "=") keyValue
    in
        case splitKeyValue of
            key :: rest ->
                case rest of
                    [] ->
                        Nothing

                    [ value ] ->
                        let
                            typeText =
                                if isObject value then
                                    "Class"
                                else
                                    determineType value

                            --cnv =
                            --NodeValue { isObject = False, nodeValue = Nothing, stringValue = (Just value) }
                        in
                            Just (Node 0 key typeText (NodeValue False (PossibleNode Nothing) (Just value)) True)

                    list ->
                        let
                            mergedList =
                                String.join "," list

                            --cnv =
                            --NodeValue { isObject = False, nodeValue = Nothing, stringValue = (Just mergedList) }
                        in
                            Just (Node 0 key (determineType mergedList) (NodeValue False (PossibleNode Nothing) (Just mergedList)) True)

            [] ->
                Nothing

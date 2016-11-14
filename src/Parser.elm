module Parser exposing (..)

import Model exposing (..)
import String exposing (..)
import Regex exposing (..)
import List exposing (..)
import Formatters exposing (determineType)


isObject : String -> Bool
isObject value =
    Regex.contains (regex objectRegex) value


objectRegex : String
objectRegex =
    -- com.company.ObjectName@1234abcd[id=1,name="QQQ"]
    "^(([a-zA-Z0-9.]+)(@[0-9a-f]{8})?)\\[(.+)\\]$"


parseNodes : RootNode -> RootNode
parseNodes rootNode =
    { rootNode | variables = (doParseNodes rootNode.variables [] 1) }


doParseNodes : List Node -> List Node -> Int -> List Node
doParseNodes nodes remaining currentId =
    let
        assignIdToNode node =
            { node | id = currentId }

        parseNode node =
            case node.value.stringValue of
                Just value ->
                    if isObject value then
                        --{ node | value = (NodeValue True (PossibleNodes (createNodes value)) Nothing) }
                        (updateNodeToObject node value currentId)
                    else
                        ( (assignIdToNode node), currentId + 1 )

                Nothing ->
                    ( node, currentId + 1 )
    in
        case nodes of
            [] ->
                remaining

            [ node ] ->
                let
                    ( updatedNode, _ ) =
                        parseNode node
                in
                    remaining ++ [ updatedNode ]

            node :: tail ->
                let
                    ( updatedNode, nextId ) =
                        parseNode node
                in
                    doParseNodes tail (remaining ++ [ updatedNode ]) nextId


createNodes : String -> String -> String -> Node
createNodes key typeText value =
    Node 0 key typeText (NodeValue (isObject value) (PossibleNodes Nothing) (Just value)) True


updateNodeToObject : Node -> String -> Int -> ( Node, Int )
updateNodeToObject node value currentId =
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
                let
                    ( childNodes, nextId ) =
                        doCreateNodes objectValue (currentId + 1)
                in
                    ( { node | id = currentId, value = (NodeValue True (PossibleNodes (Just childNodes)) Nothing) }, nextId + 1 )

            Nothing ->
                ( { node | id = currentId, value = (NodeValue False (PossibleNodes Nothing) (Just value)) }, currentId + 1 )


doCreateNodes : String -> Int -> ( List Node, Int )
doCreateNodes value currentId =
    let
        listOfKeyValuePairs =
            value
                |> splitValue

        ( childNodes, nextId ) =
            List.foldl (\kv ( result, id ) -> ( result ++ [ (createNode kv id) ], (id + 1) )) ( [], currentId ) listOfKeyValuePairs

        filteredChildNodes =
            childNodes
                |> List.filterMap (\x -> x)

        -- filterMap only keeps truthy Maybe values
    in
        ( filteredChildNodes, nextId )


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


createNode : String -> Int -> Maybe Node
createNode keyValue currentId =
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

                            ( nodes, nextId ) =
                                doCreateNodes value currentId

                            ( updatedNode, _ ) =
                                updateNodeToObject (Node 0 key typeText (NodeValue (isObject value) (PossibleNodes (Just nodes)) (Just value)) True) value nextId
                        in
                            --Just (Node 0 key typeText (NodeValue (isObject value) (PossibleNodes (createNodes value)) (Just value)) True)
                            Just updatedNode

                    list ->
                        let
                            mergedList =
                                String.join "," list

                            ( updatedNode, _ ) =
                                updateNodeToObject (Node 0 key (determineType mergedList) (NodeValue (isObject mergedList) (PossibleNodes Nothing) (Just mergedList)) True) mergedList currentId
                        in
                            --Just (Node 0 key (determineType mergedList) (NodeValue (isObject mergedList) (PossibleNodes Nothing) (Just mergedList)) True)
                            Just updatedNode

            [] ->
                Nothing

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

        -- Return the udpated node and the next id
        parseNode node =
            case node.value.stringValue of
                Just value ->
                    if isObject value then
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


updateNodeToObject : Node -> String -> Int -> ( Node, Int )
updateNodeToObject node valueInString currentId =
    let
        regexMatches =
            find All (regex objectRegex) valueInString
                |> List.map .submatches
                |> List.head

        -- Only the last part of regex matches are interesting. We invert only
        -- to get the head of this list and discard the rest
        reverseMatches =
            case regexMatches of
                Just match_list ->
                    List.reverse match_list

                Nothing ->
                    []

        possiblePropertyStringList =
            head reverseMatches
                |> Maybe.andThen (\x -> x)
    in
        case possiblePropertyStringList of
            -- Sample: prop1=1,prop2=stuff
            Just propertyStringList ->
                let
                    ( childNodes, nextId ) =
                        createChildNodes propertyStringList (currentId + 1)
                in
                    ( { node | id = currentId, value = (NodeValue True (PossibleNodes (Just childNodes)) Nothing) }, nextId )

            Nothing ->
                ( { node | id = currentId, value = (NodeValue False (PossibleNodes Nothing) (Just valueInString)) }, currentId + 1 )


reduceProperties : String -> ( List (Maybe Node), Int ) -> ( List (Maybe Node), Int )
reduceProperties kv ( result, id ) =
    let
        ( node, nextId ) =
            createNode kv id
    in
        ( result ++ [ node ], nextId )


createChildNodes : String -> Int -> ( List Node, Int )
createChildNodes propertyList currentId =
    let
        listOfKeyValuePairs =
            propertyList
                |> splitProperties

        ( childNodes, nextId ) =
            List.foldl reduceProperties ( [], currentId ) listOfKeyValuePairs

        filteredChildNodes =
            childNodes
                |> List.filterMap (\x -> x)

        -- filterMap only keeps truthy Maybe values
    in
        ( filteredChildNodes, nextId )


{-| This method intelligently splits the property list string into an actual
list.  The reason we need to do this is because properties can be objects too.
A naive split would cause sub-objects to fall apart.

Example: prop1=1,prop2=com.company.SomeEntity[prop1=2,prop2=stuff]

If we would just split by commas, prop2 could not be parsed properly
-}
splitProperties : String -> List String
splitProperties propertyList =
    let
        valueList =
            doSplitProperties propertyList []
    in
        valueList |> List.reverse


doSplitProperties : String -> List String -> List String
doSplitProperties remainingPart values =
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
                    doSplitProperties newRemainder (newValue :: values)


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


createNode : String -> Int -> ( Maybe Node, Int )
createNode keyValue currentId =
    let
        splitKeyValue =
            Regex.split (AtMost 1) (regex "=") keyValue
    in
        case splitKeyValue of
            key :: rest ->
                case rest of
                    [] ->
                        -- Do not increment the next Id because the node is Nothing
                        ( Nothing, currentId )

                    [ value ] ->
                        let
                            typeText =
                                if isObject value then
                                    "Class"
                                else
                                    determineType value

                            ( nodes, _ ) =
                                createChildNodes value currentId

                            ( updatedNode, nextId ) =
                                updateNodeToObject (Node 0 key typeText (NodeValue (isObject value) (PossibleNodes (Just nodes)) (Just value)) True) value currentId
                        in
                            ( Just updatedNode, nextId )

                    list ->
                        -- It should not get here
                        let
                            mergedList =
                                String.join "," list

                            ( updatedNode, _ ) =
                                updateNodeToObject (Node 0 key (determineType mergedList) (NodeValue (isObject mergedList) (PossibleNodes Nothing) (Just mergedList)) True) mergedList currentId
                        in
                            ( Just updatedNode, currentId + 1 )

            [] ->
                -- Do not increment the next Id because the node is Nothing
                ( Nothing, currentId )

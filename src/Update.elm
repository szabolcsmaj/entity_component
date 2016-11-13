module Update exposing (..)

import Model exposing (..)
import Message exposing (..)
import Regex exposing (..)
import Formatters exposing (determineType)
import String exposing (..)
import List exposing (..)


update : Msg -> RootNode -> ( RootNode, Cmd Msg )
update msg rootNode =
    case msg of
        LoadSuccess loadedNode ->
            ( (parseNodes loadedNode), Cmd.none )

        LoadFail error ->
            let
                _ =
                    --TODO: Remove this and handle error properly
                    Debug.log "ERROR HAPPENED" error
            in
                ( rootNode, Cmd.none )

        SwitchExtended nodeName ->
            ( { rootNode | variables = (switchExtended nodeName rootNode.variables) }, Cmd.none )


parseNodes : RootNode -> RootNode
parseNodes rootNode =
    { rootNode | variables = (doParseNodes rootNode.variables [] 1) }


doParseNodes : List Node -> List Node -> Int -> List Node
doParseNodes nodes remaining nextId =
    let
        assignIdToNode node =
            { node | id = nextId }

        parseNode node =
            case node.value.stringValue of
                Just value ->
                    if isObject value then
                        { node | value = (NodeValue True (PossibleNodes (createNodes value)) Nothing) }
                            |> assignIdToNode
                    else
                        assignIdToNode node

                Nothing ->
                    node
    in
        case nodes of
            [] ->
                remaining

            [ node ] ->
                remaining ++ [ (parseNode node) ]

            node :: tail ->
                doParseNodes tail (remaining ++ [ (parseNode node) ]) (nextId + 1)


objectRegex : String
objectRegex =
    -- com.company.ObjectName@1234abcd[id=1,name="QQQ"]
    "^(([a-zA-Z0-9.]+)(@[0-9a-f]{8})?)\\[(.+)\\]$"


isObject : String -> Bool
isObject value =
    Regex.contains (regex objectRegex) value


createNodes : String -> Maybe (List Node)
createNodes value =
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
                Just (doCreateNodes objectValue)

            Nothing ->
                Nothing


doCreateNodes : String -> List Node
doCreateNodes value =
    value
        |> splitValue
        -- filterMap only keeps truthy Maybe values
        |>
            List.filterMap createNode


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
                        in
                            Just (Node 0 key typeText (NodeValue (isObject value) (PossibleNodes (createNodes value)) (Just value)) True)

                    list ->
                        let
                            mergedList =
                                String.join "," list
                        in
                            Just (Node 0 key (determineType mergedList) (NodeValue (isObject mergedList) (PossibleNodes Nothing) (Just mergedList)) True)

            [] ->
                Nothing


switchExtended : String -> List Node -> List Node
switchExtended nodeName nodes =
    let
        select existingNode =
            if existingNode.name == nodeName then
                { existingNode | extended = not existingNode.extended }
            else
                existingNode
    in
        List.map select nodes

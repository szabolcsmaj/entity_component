module Update exposing (..)

import Model exposing (..)
import Message exposing (..)
import Regex exposing (..)


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
    { rootNode | variables = (addIdToNodes rootNode.variables) }


addIdToNodes : List Node -> List Node
addIdToNodes nodes =
    doAddIdToNodes nodes [] 1


doAddIdToNodes : List Node -> List Node -> Int -> List Node
doAddIdToNodes nodes remaining nextId =
    let
        assignIdToNode node =
            { node | id = nextId }

        parseNode node =
            case node.value.stringValue of
                Just value ->
                    if isObject value then
                        -- TODO: stringValue to false, nodeValue --> Parse!
                        { node | value = (NodeValue True (PossibleNode Nothing) Nothing) }
                            |> assignIdToNode
                    else
                        assignIdToNode node

                Nothing ->
                    node

        _ =
            Debug.log "id: " nextId
    in
        case nodes of
            [] ->
                remaining

            [ node ] ->
                remaining ++ [ (parseNode node) ]

            node :: tail ->
                doAddIdToNodes tail (remaining ++ [ (parseNode node) ]) (nextId + 1)


objectRegex : String
objectRegex =
    -- com.company.ObjectName@1234abcd[id=1,name="QQQ"]
    "^(([a-zA-Z0-9.]+)(@[0-9a-f]{8})?)\\[(.+)\\]$"


isObject : String -> Bool
isObject value =
    Regex.contains (regex objectRegex) value


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

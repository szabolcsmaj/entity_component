module Update exposing (..)

import Model exposing (..)
import Message exposing (..)


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
    case nodes of
        [] ->
            remaining

        [ node ] ->
            remaining ++ [ { node | id = nextId } ]

        node :: tail ->
            doAddIdToNodes tail (remaining ++ [ { node | id = nextId } ]) (nextId + 1)


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

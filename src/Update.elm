module Update exposing (..)

import Model exposing (..)
import Message exposing (..)


update : Msg -> RootNode -> ( RootNode, Cmd Msg )
update msg rootNode =
    case msg of
        LoadSuccess loadedNode ->
            ( loadedNode, Cmd.none )

        LoadFail error ->
            let
                _ =
                    --TODO: Remove this and handle error properly
                    Debug.log "ERROR HAPPENED" error
            in
                ( rootNode, Cmd.none )

        SwitchExtended nodeName ->
            ( { rootNode | variables = (switchExtended nodeName rootNode.variables) }, Cmd.none )


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

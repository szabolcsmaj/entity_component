module Update exposing (..)

import Model exposing (..)
import Message exposing (..)
import Regex exposing (..)
import String exposing (..)
import List exposing (..)
import Parser exposing (..)


update : Msg -> RootNode -> ( RootNode, Cmd Msg )
update msg rootNode =
    case msg of
        LoadSuccess loadedNode ->
            ( (Parser.parseNodes loadedNode), Cmd.none )

        LoadFail error ->
            let
                _ =
                    --TODO: Remove this and handle error properly
                    Debug.log "ERROR HAPPENED" error
            in
                ( rootNode, Cmd.none )

        SwitchExtended id ->
            let
                switch =
                    switchExtended id
            in
                ( { rootNode | variables = (switch rootNode.variables) }, Cmd.none )


extendNodes : Int -> Node -> Node
extendNodes id node =
    let
        switchNodes =
            extendNodes id
    in
        if node.id == id then
            { node | extended = not node.extended }
        else if node.value.isObject then
            let
                (PossibleNodes pnodes) =
                    node.value.nodeValues
            in
                case pnodes of
                    Just childNodes ->
                        let
                            value' =
                                node.value

                            newValue =
                                { value' | nodeValues = PossibleNodes ((Just (List.map switchNodes childNodes))) }
                        in
                            { node | value = newValue }

                    Nothing ->
                        node
        else
            node


switchExtended : Int -> List Node -> List Node
switchExtended id nodes =
    let
        switchNodes =
            extendNodes id
    in
        List.map switchNodes nodes

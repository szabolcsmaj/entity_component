module Update exposing (..)

import Model exposing (..)
import Message exposing (..)


update : Msg -> Node -> (Node, Cmd Msg)
update msg node =
    case msg of
        LoadSuccess loadedNode ->
            (loadedNode, Cmd.none)
        LoadFail error ->
            (node, Cmd.none)

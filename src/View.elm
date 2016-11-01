module View exposing (..)

import Html exposing (..)
import Message exposing (..)
import Model exposing (..)


view : Node -> Html Msg
view node =
    div []
        [ text "Node data:"
        , text node.class
        , text node.method
        ]

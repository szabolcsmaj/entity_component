module View exposing (..)

import Html exposing (..)
import Message exposing (..)
import Model exposing (..)


view : Node -> Html Msg
view node =
    div []
        [ text "ROOT" ]

module Main exposing (..)

import Html.App exposing (program)
import Message exposing (..)
import Model exposing (..)
import View exposing (..)
import Update exposing (..)
import Command exposing (..)

initialize : (Node, Cmd Msg)
initialize =
    (Node "" "", load)

main =
    Html.App.program 
    { init = initialize
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }


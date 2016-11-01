module Command exposing (..)

import Http
import Task
import Json.Decode as Decode exposing (..)
import Model exposing (..)
import Message exposing (..)

serverUrl : String
serverUrl = 
    "http://localhost:4000/entity"

load : Cmd Msg
load = 
    Http.get objectDecoder serverUrl
    |> Task.perform LoadFail LoadSuccess



objectDecoder : Decode.Decoder Node
objectDecoder =
    Decode.object2 Node
        ("class" := Decode.string)
        ("method" := Decode.string)

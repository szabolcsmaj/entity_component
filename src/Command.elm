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


nodeDecoder : Decode.Decoder Node
nodeDecoder =
    Decode.object3 Node
        ("name" := string)
        ("type" := string)
        ("value" := string)


nodeListDecoder : Decode.Decoder (List Node)
nodeListDecoder =
    list nodeDecoder


objectDecoder : Decode.Decoder RootNode
objectDecoder =
    Decode.object3 RootNode
        ("class" := Decode.string)
        ("method" := Decode.string)
        ("variables" := nodeListDecoder)

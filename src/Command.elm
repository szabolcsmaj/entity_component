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
    Http.get serverUrl objectDecoder
        |> Http.send Loaded


nodeDecoder : Decode.Decoder Node
nodeDecoder =
    Decode.map5 Node
        (succeed 0)
        (field "name" string)
        (field "type" string)
        nodeValueDecoder
        (succeed True)


nodeValueDecoder : Decode.Decoder NodeValue
nodeValueDecoder =
    Decode.map3
        NodeValue
        (succeed False)
        (succeed (PossibleNodes Nothing))
        (field "value" (maybe string))


nodeListDecoder : Decode.Decoder (List Node)
nodeListDecoder =
    list nodeDecoder


objectDecoder : Decode.Decoder RootNode
objectDecoder =
    Decode.map3 RootNode
        (field "class" Decode.string)
        (field "method" Decode.string)
        (field "variables" nodeListDecoder)

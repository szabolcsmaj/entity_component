module Model exposing (..)

type NodeType =
    NObject
    | NList

type alias Node =
    { class : String
    , method : String
    }

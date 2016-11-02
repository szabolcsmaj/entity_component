module Model exposing (..)


type NodeType
    = NObject
    | NList


type alias RootNode =
    { class : String
    , method : String
    , variables : List Node
    }


type alias Node =
    { name : String
    , nodeType : String
    , value : String
    }

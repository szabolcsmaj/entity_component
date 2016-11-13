module Model exposing (..)


type NodeType
    = NObject
    | NList


type alias RootNode =
    { class : String
    , method : String
    , variables : List Node
    }


type alias NodeValue =
    { isObject : Bool
    , nodeValues : PossibleNodes
    , stringValue : Maybe String
    }


type PossibleNodes
    = PossibleNodes (Maybe (List Node))


type alias Node =
    { id : Int
    , name : String
    , nodeType : String
    , value : NodeValue
    , extended : Bool
    }


type alias KeyValuePair =
    { key : String
    , value : String
    }

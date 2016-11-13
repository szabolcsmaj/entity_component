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
    , nodeValue : PossibleNode
    , stringValue : Maybe String
    }


type PossibleNode
    = PossibleNode (Maybe Node)


type alias Node =
    { id : Int
    , name : String
    , nodeType : String
    , value : NodeValue
    , extended : Bool
    }

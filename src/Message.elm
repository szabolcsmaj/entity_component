module Message exposing (..)

import Http
import Model exposing (..)


type Msg
    = Loaded (Result Http.Error RootNode)
    | SwitchExtended Int

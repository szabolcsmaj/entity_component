module Message exposing (..)

import Http
import Model exposing (..)


type Msg
    = LoadSuccess RootNode
    | LoadFail Http.Error
    | SwitchExtended String

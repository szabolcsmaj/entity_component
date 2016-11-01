module Message exposing (..)

import Http
import Model exposing (..)

type Msg =
    LoadSuccess Node
    | LoadFail Http.Error

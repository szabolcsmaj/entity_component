port module Main exposing (..)

import ViewTests
import Test.Runner.Node exposing (run)
import Json.Encode exposing (Value)


main : Program Value
main =
    run emit ViewTests.all


port emit : ( String, Value ) -> Cmd msg

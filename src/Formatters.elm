module Formatters exposing (determineType)

import String
import Regex


--import Date

import Date.Extra as Date
import Debug


determineType : String -> String
determineType value =
    case
        (Maybe.oneOf
            [ (parseInt value)
            , (parseFloat value)
            , (parseDate value)
            , (Just "String")
            ]
        )
    of
        Just determinedType ->
            determinedType

        Nothing ->
            "Unknown"


parseInt : String -> Maybe String
parseInt value =
    case (String.toInt value) of
        Ok _ ->
            Just "Integer"

        Err _ ->
            Nothing


parseFloat : String -> Maybe String
parseFloat value =
    case (String.toFloat value) of
        Ok _ ->
            Just "Float"

        Err _ ->
            Nothing


parseDate : String -> Maybe String
parseDate value =
    if (Regex.contains (Regex.regex "^[12][0-9]{3}-[0-9]{1,2}-[0-9]{1,2} [0-9]{2}:[0-9]{2}:[0-9]{2}.[0-9]{1,6}$") value) then
        Just "Date"
    else
        Nothing

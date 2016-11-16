module Formatters exposing (determineType)

import String
import Regex


determineType : String -> String
determineType value =
    case
        (oneOf
            [ (parseInt value)
            , (parseFloat value)
            , (parseDate value)
            , (parseBool value)
            , (Just "String")
            ]
        )
    of
        Just determinedType ->
            determinedType

        Nothing ->
            "Unknown"


oneOf : List (Maybe String) -> Maybe String
oneOf maybes =
    case maybes of
        [] ->
            Nothing

        maybe :: rest ->
            case maybe of
                Nothing ->
                    oneOf rest

                Just _ ->
                    maybe


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


parseBool : String -> Maybe String
parseBool value =
    let
        boolText =
            "Boolean"
    in
        case (String.toLower value) of
            "false" ->
                Just boolText

            "true" ->
                Just boolText

            _ ->
                Nothing

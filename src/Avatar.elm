module Avatar exposing (Avatar, decoder, encode, selectionSet, view)

import Asset
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)



-- TYPES


type Avatar
    = Avatar (Maybe String)



-- CREATE


decoder : Decoder Avatar
decoder =
    Decode.map Avatar (Decode.nullable Decode.string)



-- TRANSFORM


encode : Avatar -> Value
encode (Avatar maybeUrl) =
    case maybeUrl of
        Just url ->
            Encode.string url

        Nothing ->
            Encode.null



--src : Avatar -> Attribute msg
--src (Avatar maybeUrl) =
--    case maybeUrl of
--        Nothing ->
--            Asset.src Asset.defaultAvatar
--
--        Just "" ->
--            Asset.src Asset.defaultAvatar
--
--        Just url ->
--            Html.Attributes.src url


view : Avatar -> Int -> Element msg
view (Avatar maybeString) size =
    el
        [ width (px size)
        , height (px size)
        , Border.rounded <| round (toFloat size / 2)
        , Background.image <|
            Asset.src <|
                case maybeString of
                    Nothing ->
                        Asset.defaultAvatar

                    Just src ->
                        Asset.avatar src
        ]
        Element.none


toMaybeString : Avatar -> Maybe String
toMaybeString (Avatar maybeUrl) =
    maybeUrl


selectionSet : SelectionSet (Maybe String) typeLock -> SelectionSet Avatar typeLock
selectionSet maybeSting =
    SelectionSet.map Avatar maybeSting

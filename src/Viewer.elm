module Viewer exposing (Viewer, cred, decoder, info, minPasswordChars, selectionSet, store)

{-| The logged-in user currently viewing this page. It stores enough data to
be able to render the menu bar (username and avatar), along with Cred so it's
impossible to have a Viewer if you aren't logged in.
-}

import API.Object
import Api exposing (Cred)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom)
import User exposing (User)



-- TYPES


type Viewer
    = Viewer User Cred



-- INFO


cred : Viewer -> Cred
cred (Viewer _ val) =
    val


info : Viewer -> User
info (Viewer val _) =
    val


{-| Passwords must be at least this many characters long!
-}
minPasswordChars : Int
minPasswordChars =
    8



-- SERIALIZATION


decoder : Decoder (Cred -> Viewer)
decoder =
    Decode.succeed Viewer
        |> custom (Decode.field "info" User.decoder)


selectionSet :
    (SelectionSet User API.Object.User -> SelectionSet User typeLock)
    -> (SelectionSet Cred typeLock -> SelectionSet Viewer typeLock)
selectionSet userToTypeLock =
    SelectionSet.map2 Viewer (userToTypeLock User.selectionSet)


store : Viewer -> Cmd msg
store (Viewer infoVal credVal) =
    Api.storeCredWith
        credVal
        infoVal

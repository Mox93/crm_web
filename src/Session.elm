module Session exposing (Session, changes, config, cred, fromViewer, navKey, updateMeta, user, viewer)

import Api exposing (Cred)
import Browser.Navigation as Nav
import Config exposing (Config)
import User exposing (User)
import Viewer exposing (Viewer)



-- TYPES


type Session
    = Guest Config Nav.Key
    | LoggedIn Config Nav.Key Viewer



-- INFO


config : Session -> Config
config session =
    case session of
        LoggedIn val _ _ ->
            val

        Guest val _ ->
            val


viewer : Session -> Maybe Viewer
viewer session =
    case session of
        LoggedIn _ _ val ->
            Just val

        Guest _ _ ->
            Nothing


cred : Session -> Maybe Cred
cred session =
    case session of
        LoggedIn _ _ val ->
            Just (Viewer.cred val)

        Guest _ _ ->
            Nothing


user : Session -> Maybe User
user session =
    case session of
        LoggedIn _ _ val ->
            Just (Viewer.info val)

        Guest _ _ ->
            Nothing


navKey : Session -> Nav.Key
navKey session =
    case session of
        Guest _ key ->
            key

        LoggedIn _ key _ ->
            key



-- CHANGES


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    Api.viewerChanges (\maybeViewer -> toMsg (fromViewer key maybeViewer)) Viewer.decoder


updateMeta : Session -> Config -> Session
updateMeta session newMeta =
    case session of
        Guest _ key ->
            Guest newMeta key

        LoggedIn _ key val ->
            LoggedIn newMeta key val


fromViewer : Nav.Key -> Maybe Viewer -> Session
fromViewer key maybeViewer =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    case maybeViewer of
        Just viewerVal ->
            LoggedIn Config.defaults key viewerVal

        Nothing ->
            Guest Config.defaults key

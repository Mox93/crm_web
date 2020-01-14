module Route exposing (Route(..), fromUrl, href, replaceUrl, toString)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attributes
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, fragment, int, map, oneOf, s, string, top)



-- ROUTING


type Route
    = Home
    | Root
    | Login
    | Logout
    | Signup
    | Welcome
    | MyProfile


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map Login (s "login")
        , map Logout (s "logout")
        , map Signup (s "signup")
        , map Welcome (s "welcome")
        , map MyProfile (s "profile")
        ]



-- PUBLIC HELPERS


href : Route -> Attribute msg
href targetRoute =
    Attributes.href (toString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (toString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url



-- INTERNAL


toString : Route -> String
toString page =
    "/" ++ String.join "/" (routeToPieces page)


routeToPieces : Route -> List String
routeToPieces page =
    case page of
        Home ->
            []

        Root ->
            []

        Login ->
            [ "login" ]

        Logout ->
            [ "logout" ]

        Signup ->
            [ "signup" ]

        Welcome ->
            [ "welcome" ]

        MyProfile ->
            [ "profile" ]

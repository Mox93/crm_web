module Route exposing (Route(..), fromUrl, href, replaceUrl, toString)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attributes
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), (<?>), Parser, map, oneOf, s, top)



-- ROUTING


type Route
    = Root
    | Home
    | Login
    | Logout
    | Signup
    | Welcome
    | MyProfile


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Root top
        , map Home (s "home")
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
        Root ->
            []

        Home ->
            [ "home" ]

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

module Page exposing (Page(..), view)

import Brand
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Layout
import Route
import User
import Viewer exposing (Viewer)


type Page
    = Other
    | Home
    | Login
    | Signup
    | Welcome
    | MyProfile
    | NotFound


view : Maybe Viewer -> Page -> { title : String, body : Element msg } -> Browser.Document msg
view maybeViewer page { title, body } =
    { title = "rizzmi/" ++ title
    , body =
        [ layout
            [ appBar maybeViewer page
                |> inFront
            , Brand.defaultBodyPadding
            , Background.color Brand.canvasColor
            , Brand.defaultFont
            ]
            body
        ]
    }


appBar : Maybe Viewer -> Page -> Element msg
appBar maybeViewer page =
    row
        [ width fill
        , height (Element.px Brand.appBarHeight)
        , Brand.defaultBarPadding
        , Background.color Brand.secondaryColor
        , Font.color Brand.primaryTextColorDBg
        , Brand.shadow
        ]
        [ link [ Font.bold ]
            { url = Route.toString Route.Root
            , label =
                image
                    [ height (px <| Brand.scaled 3) ]
                    { src = "/web/static/assets/logo.svg"
                    , description = "rizzmi"
                    }
            }
        , row
            [ alignRight
            , spacing <| Brand.scaled 2
            ]
          <|
            case maybeViewer of
                Nothing ->
                    viewEntrance page

                Just viewer ->
                    [ viewAvatar page viewer
                    ]
        ]


viewEntrance : Page -> List (Element msg)
viewEntrance page =
    case page of
        Login ->
            [ signupBtn ]

        Signup ->
            [ loginBtn ]

        Home ->
            [ signupBtn
            , loginBtn
            ]

        _ ->
            []


viewAvatar : Page -> Viewer -> Element msg
viewAvatar page viewer =
    case page of
        MyProfile ->
            logoutBtn

        _ ->
            link []
                { url = Route.toString Route.MyProfile
                , label =
                    image
                        [ width (px 32)
                        , height (px 32)
                        ]
                        { src = "/web/static/assets/avatar.svg"
                        , description = "avatar"
                        }
                }


signupBtn : Element msg
signupBtn =
    link []
        { url = Route.toString Route.Signup
        , label = Layout.primaryBtn <| text "Sign up for free"
        }


loginBtn : Element msg
loginBtn =
    link []
        { url = Route.toString Route.Login
        , label = Layout.secondaryBan <| text "Login"
        }


logoutBtn : Element msg
logoutBtn =
    link []
        { url = Route.toString Route.Logout
        , label = Layout.warningBtn <| text "Logout"
        }

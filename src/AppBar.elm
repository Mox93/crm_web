module AppBar exposing (Menu(..), Msg, Page(..), update, view)

import Asset
import Avatar exposing (Avatar)
import Brand
import Config exposing (Config)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Layout
import Route
import Session exposing (Session)



-- TYPES


type Page
    = Other
    | Home
    | Login
    | Signup
    | Welcome
    | MyProfile


type Menu
    = MainMenu
    | ConfigMenu
    | NotificationMenu



{-
   type alias GuestState =
       { config : Config
       }


   type alias UserState =
       { avatar : Avatar
       , notifications : Int
       , config : Config
       }


   type State
       = GuestBar -- GuestState
       | UserBar -- UserState


   init : Session -> ( State, Cmd Msg )
   init session =
       ( fromSession session
       , Cmd.none
       )


   fromSession : Session -> State
   fromSession session =
       let
           maybeViewer =
               Session.viewer session

           config =
               Session.config session
       in
       case maybeViewer of
           Nothing ->
               GuestBar

           -- { config = config }
           Just viewer ->
               UserBar



    <|
   let
       avatar =
           Viewer.avatar viewer
   in
   { avatar = avatar
   , notifications = 0
   , config = config
   }
-}
-- UPDATE


type Msg
    = ToggleMenu Menu


update : Msg -> ( Cmd Msg, Maybe Menu )
update msg =
    case msg of
        ToggleMenu menu ->
            ( Cmd.none, Just menu )



-- SUBSCRIPTIONS
-- VIEW


view : Session -> Maybe Menu -> Page -> Element Msg
view session maybeMenu page =
    let
        config =
            Session.config session

        maybeUser =
            Session.user session
    in
    row
        [ width fill
        , height (px Brand.appBarHeight)
        , Brand.defaultBarPadding
        , Background.color Brand.secondaryColor
        , Font.color Brand.primaryTextColorDBg
        , Brand.shadow
        ]
        [ viewLogo
        , row
            [ alignRight
            , spacing <| Brand.defaultPaddingAmount
            ]
          <|
            case maybeUser of
                Nothing ->
                    List.append
                        [ configBtn config (maybeMenu == Just ConfigMenu) ]
                    <|
                        viewEntrance page

                Just user ->
                    viewLinks 0 config user.avatar maybeMenu
        ]


viewLogo : Element msg
viewLogo =
    link [ Font.bold ]
        { url = Route.toString Route.Root
        , label =
            image
                [ height (px <| Brand.scaled 3) ]
                { src = Asset.src Asset.logo
                , description = "rizzmi"
                }
        }


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


viewLinks : Int -> Config -> Avatar -> Maybe Menu -> List (Element Msg)
viewLinks notificationCount config avatar maybeMenu =
    [ row
        [ spacing <| Brand.scaled -1 ]
        [ notificationsBtn notificationCount (maybeMenu == Just NotificationMenu)
        , configBtn config (maybeMenu == Just ConfigMenu)
        ]
    , avatarBtn avatar (maybeMenu == Just MainMenu)
    ]


notificationsBtn : Int -> Bool -> Element Msg
notificationsBtn num openMenu =
    el
        (if openMenu then
            [ below notificationsMenu ]

         else
            []
        )
        (Input.button
            [ centerY
            , Border.rounded 12
            , mouseOver [ Background.color Brand.cardColor ]
            , focused []

            --, onLoseFocus (ToggleMenu NotificationMenu)
            ]
            { onPress = Just (ToggleMenu NotificationMenu)
            , label =
                image
                    [ width (px 24)
                    , height (px 24)
                    ]
                    { src = Asset.src Asset.bell
                    , description = "notifications"
                    }
            }
        )


notificationsMenu : Element msg
notificationsMenu =
    column
        [ Background.color Brand.secondaryColor
        , Font.color Brand.primaryTextColorDBg
        , Font.size 16
        , Border.rounded 8
        , moveDown 20
        , centerX
        , clip
        , width (px <| Brand.scaled 13)
        ]
        [ paragraph [ padding 12 ] [ text "Not implemented yet." ] ]


configBtn : Config -> Bool -> Element Msg
configBtn config openMenu =
    el
        (if openMenu then
            [ below configMenu ]

         else
            []
        )
        (Input.button
            [ centerY
            , Border.rounded 12
            , mouseOver [ Background.color Brand.cardColor ]
            , focused []

            --, onLoseFocus (ToggleMenu ConfigMenu)
            ]
            { onPress = Just (ToggleMenu ConfigMenu)
            , label =
                image
                    [ width (px 24)
                    , height (px 24)
                    ]
                    { src = Asset.src Asset.gear
                    , description = "config"
                    }
            }
        )


configMenu : Element msg
configMenu =
    column
        [ Background.color Brand.secondaryColor
        , Font.color Brand.primaryTextColorDBg
        , Font.size 16
        , Border.rounded 8
        , moveDown 20
        , centerX
        , clip
        , width shrink
        , spacing 8
        , padding 8
        ]
        [ themeSelector
        , languageSelector
        ]


themeSelector : Element msg
themeSelector =
    row
        [ spacing 16
        , padding 8
        , centerX
        , Background.color Brand.secondaryColorMuted
        , Border.rounded 4
        ]
        [ el
            [ width (px 24)
            , height (px 24)
            , Border.rounded 12
            , Background.color Brand.cardColor
            ]
            Element.none
        , el
            [ width (px 24)
            , height (px 24)
            , Border.rounded 12
            , Background.color Brand.secondaryColor
            ]
            Element.none
        ]


languageSelector : Element msg
languageSelector =
    row
        [ spacing 16
        , padding 8
        , centerX
        , Background.color Brand.secondaryColorMuted
        , Border.rounded 4
        , Font.bold
        ]
        [ el
            [ width (px 24)
            , height (px 24)
            , Border.rounded 4
            ]
          <|
            el
                [ centerX
                , centerY
                ]
            <|
                text "En"
        , el
            [ width (px 24)
            , height (px 24)
            , Border.rounded 4
            ]
          <|
            el
                [ centerX
                , alignTop
                ]
            <|
                text "ع"
        ]


avatarBtn : Avatar -> Bool -> Element Msg
avatarBtn avatar openMenu =
    el
        (if openMenu then
            [ below mainMenu ]

         else
            []
        )
        (Input.button
            [ centerY
            , Border.rounded 16
            , focused []

            --, onLoseFocus (ToggleMenu MainMenu)
            ]
            { onPress = Just (ToggleMenu MainMenu)
            , label = Avatar.view avatar 32
            }
        )


mainMenu : Element msg
mainMenu =
    column
        [ Background.color Brand.secondaryColor
        , Font.color Brand.primaryTextColorDBg
        , Border.rounded 8
        , moveDown 16
        , moveRight 8
        , alignRight
        , clip
        , width (px <| Brand.scaled 11)
        ]
        [ myProfileLink
        , divider
        , editLink
        , settingsLink
        , divider
        , logoutLink
        ]


myProfileLink : Element msg
myProfileLink =
    menuLink
        { url = Route.toString Route.MyProfile
        , label = text "My Profile"
        }


editLink : Element msg
editLink =
    menuLink
        { url = "#"
        , label = text "Edit"
        }


settingsLink : Element msg
settingsLink =
    menuLink
        { url = "#"
        , label = text "Settings"
        }


logoutLink : Element msg
logoutLink =
    menuLink
        { url = Route.toString Route.Logout
        , label = text "Logout"
        }


menuLink : { url : String, label : Element msg } -> Element msg
menuLink prop =
    link
        [ width fill
        , padding 12
        , Font.size 16
        , mouseOver [ Background.color Brand.secondaryColorMuted ]
        ]
        prop


divider : Element msg
divider =
    el
        [ width fill
        , height fill
        , paddingXY 12 4
        ]
    <|
        el
            [ height (px 2)
            , width fill
            , Background.color Brand.secondaryColorMuted
            , width fill
            ]
            Element.none


signupBtn : Element msg
signupBtn =
    link [ centerY ]
        { url = Route.toString Route.Signup
        , label = Layout.primaryBtn <| text "Sign up for free"
        }


loginBtn : Element msg
loginBtn =
    link [ centerY ]
        { url = Route.toString Route.Login
        , label = Layout.secondaryBan <| text "Login"
        }

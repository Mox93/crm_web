module Page.Home exposing (Model, Msg, init, toSession, update, view)

import Brand
import Browser
import Element exposing (..)
import Element.Background as Background
import Html as H
import Layout
import Meta exposing (Language(..), Meta, Theme)
import Route exposing (Route)
import Session exposing (Session, meta)
import Tabs exposing (Tabs)



-- MODEL


type Section
    = Main
    | Features
    | Pricing
    | ContactUs


type alias Model =
    { session : Session
    , tabs : Tabs Section
    }


init : Session -> ( Model, Cmd msg )
init session =
    let
        tabs =
            Tabs [] Main [ Features, Pricing, ContactUs ]
    in
    ( Model session tabs, Cmd.none )



-- UPDATE


type Msg
    = ChangeFocus Section
    | ChangeLanguage Language
    | ChangeTheme Theme


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeFocus section ->
            ( { model | tabs = Tabs.switchTo model.tabs section }
            , Cmd.none
            )

        ChangeLanguage lang ->
            let
                meta =
                    Session.meta model.session

                session =
                    { meta | language = lang }
                        |> Session.updateMeta model.session
            in
            init session

        ChangeTheme theme ->
            let
                meta =
                    Session.meta model.session

                session =
                    { meta | theme = theme }
                        |> Session.updateMeta model.session
            in
            init session



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = sectionName model.tabs.current
    , body =
        List.map viewContent model.tabs.before
            ++ [ viewContent model.tabs.current ]
            ++ List.map viewContent model.tabs.after
    }


viewContent : Section -> H.Html msg
viewContent section =
    case section of
        Main ->
            viewMain "Welcome to Our Website"

        Features ->
            viewFeatures "A Lot of Awesome Stuff"

        Pricing ->
            viewPricing "Not so Expensive"

        ContactUs ->
            viewContactUS "000-000-0000"


viewMain : String -> H.Html msg
viewMain main =
    layout
        [ Brand.defaultFont ]
    <|
        viewDark main


viewFeatures : String -> H.Html msg
viewFeatures features =
    layout
        [ Brand.defaultFont ]
    <|
        viewLight features


viewPricing : String -> H.Html msg
viewPricing pricing =
    layout
        [ Brand.defaultFont ]
    <|
        viewDark pricing


viewContactUS : String -> H.Html msg
viewContactUS contactUs =
    layout
        [ inFront <| Layout.appBar appBarContent
        , Brand.defaultFont
        ]
    <|
        viewLight contactUs


viewDark : String -> Element msg
viewDark title =
    row
        [ Background.color Brand.canvasColor
        , height fill
        , width fill
        , Brand.defaultBodyPadding
        ]
        [ el [ centerX ] <| text title ]


viewLight : String -> Element msg
viewLight title =
    row
        [ Background.color Brand.cardColor
        , height fill
        , width fill
        , Brand.defaultBodyPadding
        ]
        [ el [ centerX ] <| text title ]


appBarContent : Element msg
appBarContent =
    row
        [ alignRight
        , spacing <| Brand.scaled 2
        ]
        [ link []
            { url = Route.toString Route.Signup
            , label = Layout.signupBtn
            }
        , link []
            { url = Route.toString Route.Login
            , label = Layout.loginBtn
            }
        ]


sectionName : Section -> String
sectionName sec =
    case sec of
        Main ->
            "Home"

        Features ->
            "Features"

        Pricing ->
            "Pricing"

        ContactUs ->
            "Contact Us"



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session

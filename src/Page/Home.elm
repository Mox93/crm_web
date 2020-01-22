module Page.Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Brand
import Browser
import Config exposing (Config, Language(..), Theme)
import Element exposing (..)
import Element.Background as Background
import Html as H
import Layout
import Route exposing (Route)
import Session exposing (Session, config)
import Tabs exposing (Tabs)
import User
import Viewer



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
    | GotSession Session


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
                    Session.config model.session

                session =
                    { meta | language = lang }
                        |> Session.updateMeta model.session
            in
            init session

        ChangeTheme theme ->
            let
                meta =
                    Session.config model.session

                session =
                    { meta | theme = theme }
                        |> Session.updateMeta model.session
            in
            init session

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- VIEW


view : Model -> { title : String, body : Element Msg }
view model =
    { title = sectionName model.tabs.current
    , body =
        column
            [ width fill
            ]
        <|
            List.map (viewContent model.session) model.tabs.before
                ++ [ viewContent model.session model.tabs.current ]
                ++ List.map (viewContent model.session) model.tabs.after
    }


viewContent : Session -> Section -> Element msg
viewContent session section =
    case section of
        Main ->
            viewMain "Welcome to Our Website"

        Features ->
            viewFeatures "A Lot of Awesome Stuff"

        Pricing ->
            viewPricing "Not so Expensive"

        ContactUs ->
            viewContactUS "000-000-0000"


viewMain : String -> Element msg
viewMain main =
    viewDark main


viewFeatures : String -> Element msg
viewFeatures features =
    viewLight features


viewPricing : String -> Element msg
viewPricing pricing =
    viewDark pricing


viewContactUS : String -> Element msg
viewContactUS contactUs =
    viewLight contactUs


viewDark : String -> Element msg
viewDark title =
    row
        [ Background.color Brand.canvasColor
        , height (px 600)
        , width fill
        ]
        [ el [ centerX ] <| text title ]


viewLight : String -> Element msg
viewLight title =
    row
        [ Background.color Brand.cardColor
        , height (px 600)
        , width fill
        ]
        [ el [ centerX ] <| text title ]


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

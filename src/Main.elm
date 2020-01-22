module Main exposing (..)

import Api exposing (Cred)
import AppBar exposing (Page(..))
import Browser
import Browser.Navigation as Nav
import Html as H
import Json.Decode exposing (Value)
import Page
import Page.Home as Home
import Page.Login as Login
import Page.MyProfile as MyProfile
import Page.NotFound as NotFound
import Page.Signup as Signup
import Page.Welcome as Welcome
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)
import Viewer exposing (Viewer)



-- MAIN


main : Program Value Model Msg
main =
    Api.application Viewer.decoder
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type Menu
    = FromAppBar AppBar.Menu


type Body
    = Redirect Session
    | Home Home.Model
    | Login Login.Model
    | Signup Signup.Model
    | Welcome Welcome.Model
    | MyProfile MyProfile.Model
    | NotFound NotFound.Model


type alias Model =
    { body : Body

    --, appBar : AppBar.State
    , openMenu : Maybe Menu
    }


init : Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    let
        session =
            Session.fromViewer navKey maybeViewer

        --( appBer, _ ) =
        --    AppBar.init session
    in
    changeRouteTo (Route.fromUrl url)
        { body = Redirect session

        --, appBar = appBer
        , openMenu = Nothing
        }



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotHomeMsg Home.Msg
    | GotLoginMsg Login.Msg
    | GotSignupMsg Signup.Msg
    | GotWelcomeMsg Welcome.Msg
    | GotMyProfileMsg MyProfile.Msg
    | GotNotFoundMsg NotFound.Msg
    | GotAppBarMsg AppBar.Msg
    | GotSession Session
    | ToggleMenu (Maybe Menu)


toSession : Body -> Session
toSession body =
    case body of
        Redirect session ->
            session

        Home home ->
            Home.toSession home

        Login login ->
            Login.toSession login

        Signup signup ->
            Signup.toSession signup

        Welcome welcome ->
            Welcome.toSession welcome

        MyProfile profile ->
            MyProfile.toSession profile

        NotFound notFound ->
            NotFound.toSession notFound


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model.body

        maybeViewer =
            Session.viewer session
    in
    case ( maybeRoute, maybeViewer ) of
        -- For Everyone
        --      * Root
        ( Just Route.Root, Nothing ) ->
            Home.init session
                |> updateWith model Home GotHomeMsg

        ( Just Route.Root, Just viewer ) ->
            case (Viewer.info viewer).company of
                Nothing ->
                    ( model
                    , Route.replaceUrl (Session.navKey session) Route.Welcome
                    )

                Just company ->
                    -- TODO should render the Dashboard or something
                    ( model
                    , Route.replaceUrl (Session.navKey session) Route.MyProfile
                    )

        --      * Home
        ( Just Route.Home, _ ) ->
            Home.init session
                |> updateWith model Home GotHomeMsg

        --      * 404 Page Not Found
        ( Nothing, _ ) ->
            NotFound.init session
                |> updateWith model NotFound GotNotFoundMsg

        -- Strictly For Guests
        --      * Login
        ( Just Route.Login, Nothing ) ->
            Login.init session
                |> updateWith model Login GotLoginMsg

        ( Just Route.Login, Just viewer ) ->
            ( model
            , Route.replaceUrl (Session.navKey session) Route.Root
            )

        --      * Signup
        ( Just Route.Signup, Nothing ) ->
            Signup.init session
                |> updateWith model Signup GotSignupMsg

        ( Just Route.Signup, Just viewer ) ->
            ( model
            , Route.replaceUrl (Session.navKey session) Route.Root
            )

        -- Strictly For Users
        --      * Welcome
        ( Just Route.Welcome, Nothing ) ->
            ( model
            , Route.replaceUrl (Session.navKey session) Route.Root
            )

        ( Just Route.Welcome, Just viewer ) ->
            case (Viewer.info viewer).company of
                Nothing ->
                    Welcome.init session
                        |> updateWith model Welcome GotWelcomeMsg

                Just company ->
                    ( model
                    , Route.replaceUrl (Session.navKey session) Route.Root
                    )

        --      * My Profile
        ( Just Route.MyProfile, Nothing ) ->
            ( model
            , Route.replaceUrl (Session.navKey session) Route.Root
            )

        ( Just Route.MyProfile, Just viewer ) ->
            MyProfile.init session
                |> updateWith model MyProfile GotMyProfileMsg

        --      * Logout
        ( Just Route.Logout, Nothing ) ->
            ( model
            , Route.replaceUrl (Session.navKey session) Route.Root
            )

        ( Just Route.Logout, Just viewer ) ->
            ( model
            , Api.logout
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.body ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (Session.navKey (toSession model.body)) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith model Home GotHomeMsg

        ( GotLoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith model Login GotLoginMsg

        ( GotSignupMsg subMsg, Signup signup ) ->
            Signup.update subMsg signup
                |> updateWith model Signup GotSignupMsg

        ( GotWelcomeMsg subMsg, Welcome welcome ) ->
            Welcome.update subMsg welcome
                |> updateWith model Welcome GotWelcomeMsg

        ( GotMyProfileMsg subMsg, MyProfile profile ) ->
            MyProfile.update subMsg profile
                |> updateWith model MyProfile GotMyProfileMsg

        ( GotNotFoundMsg subMsg, NotFound notFound ) ->
            NotFound.update subMsg notFound
                |> updateWith model NotFound GotNotFoundMsg

        ( GotAppBarMsg subMsg, _ ) ->
            let
                ( subCmd, maybeMenu ) =
                    AppBar.update subMsg
            in
            ( { model
                | openMenu = updateOpenMenu model.openMenu maybeMenu
              }
            , Cmd.map GotAppBarMsg subCmd
            )

        ( GotSession session, Redirect _ ) ->
            ( { model | body = Redirect session }
            , Route.replaceUrl (Session.navKey session) Route.Root
            )

        ( ToggleMenu maybeMenu, _ ) ->
            ( { model | openMenu = maybeMenu }
            , Cmd.none
            )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith :
    Model
    -> (subModel -> Body)
    -> (subMsg -> Msg)
    -> ( subModel, Cmd subMsg )
    -> ( Model, Cmd Msg )
updateWith model toBody toMsg ( subModel, subCmd ) =
    ( { model
        | body = toBody subModel
        , openMenu = Nothing
      }
    , Cmd.map toMsg subCmd
    )


updateOpenMenu : Maybe Menu -> Maybe AppBar.Menu -> Maybe Menu
updateOpenMenu maybeMenu maybeSubMenu =
    case maybeSubMenu of
        Nothing ->
            maybeMenu

        Just menu ->
            if Just (FromAppBar menu) == maybeMenu then
                Nothing

            else
                Just (FromAppBar menu)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.body of
        Redirect session ->
            Session.changes GotSession (Session.navKey session)

        NotFound notFound ->
            Sub.map GotNotFoundMsg (NotFound.subscriptions notFound)

        Home home ->
            Sub.map GotHomeMsg (Home.subscriptions home)

        Login login ->
            Sub.map GotLoginMsg (Login.subscriptions login)

        Signup signup ->
            Sub.map GotSignupMsg (Signup.subscriptions signup)

        Welcome welcome ->
            Sub.map GotWelcomeMsg (Welcome.subscriptions welcome)

        MyProfile profile ->
            Sub.map GotMyProfileMsg (MyProfile.subscriptions profile)



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        session =
            toSession model.body

        appBarMenu =
            case model.openMenu of
                Nothing ->
                    Nothing

                Just (FromAppBar menu) ->
                    Just menu

        viewPage toMsg page content =
            Page.view session appBarMenu page content GotAppBarMsg toMsg
    in
    case model.body of
        Home home ->
            viewPage GotHomeMsg AppBar.Home (Home.view home)

        Login login ->
            viewPage GotLoginMsg AppBar.Login (Login.view login)

        Signup signup ->
            viewPage GotSignupMsg AppBar.Signup (Signup.view signup)

        Welcome welcome ->
            viewPage GotWelcomeMsg AppBar.Welcome (Welcome.view welcome)

        MyProfile profile ->
            viewPage GotMyProfileMsg AppBar.MyProfile (MyProfile.view profile)

        NotFound notFound ->
            viewPage GotNotFoundMsg AppBar.Other (NotFound.view notFound)

        Redirect _ ->
            { title = "rizzmi/Loading"
            , body = [ H.h3 [] [ H.text "Loading..." ] ]
            }

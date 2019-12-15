module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Html as H
import Json.Decode as Decode
import Page.Home as Home
import Page.Login as Login
import Page.NotFound as NotFound
import Page.Signup as Signup
import Page.Welcome as Welcome
import Route exposing (Route)
import Session exposing (Session)
import Url exposing (Url)



-- MAIN


main : Program Decode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


type Model
    = Redirect Session
    | Home Home.Model
    | Login Login.Model
    | Signup Signup.Model
    | Welcome Welcome.Model
    | NotFound Session
    | NotImplemented Session


init : Decode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    -- Maybe Viewer
    Session.fromViewer key Nothing
        |> Redirect
        |> changeRouteTo (Route.fromUrl url)



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotHomeMsg Home.Msg
    | GotLoginMsg Login.Msg
    | GotSignupMsg Signup.Msg
    | GotWelcomeMsg Welcome.Msg
    | GotSession Session
    | DoNothing


toSession : Model -> Session
toSession page =
    case page of
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

        NotFound session ->
            session

        NotImplemented session ->
            session


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( NotFound session, Cmd.none )

        Just Route.Home ->
            let
                ( home, _ ) =
                    Home.init session
            in
            ( Home home, Cmd.none )

        Just Route.Login ->
            let
                ( login, _ ) =
                    Login.init session
            in
            ( Login login, Cmd.none )

        Just Route.Signup ->
            let
                ( signup, _ ) =
                    Signup.init session
            in
            ( Signup signup, Cmd.none )

        Just Route.Logout ->
            ( NotImplemented session, Cmd.none )

        Just Route.Welcome ->
            let
                ( welcome, _ ) =
                    Welcome.init (Welcome.Welcome session)
            in
            ( Welcome welcome, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( GotHomeMsg subMsg, Home home ) ->
            Home.update subMsg home
                |> updateWith Home GotHomeMsg

        ( GotLoginMsg subMsg, Login login ) ->
            Login.update subMsg login
                |> updateWith Login GotLoginMsg

        ( GotSignupMsg subMsg, Signup signup ) ->
            Signup.update subMsg signup
                |> updateWith Signup GotSignupMsg

        ( GotWelcomeMsg subMsg, Welcome welcome ) ->
            Welcome.update subMsg welcome
                |> updateWith Welcome GotWelcomeMsg

        ( GotSession session, Redirect _ ) ->
            ( Redirect session
            , Route.replaceUrl (Session.navKey session) Route.Home
            )

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    case model of
        Home home ->
            let
                { title, body } =
                    Home.view home
            in
            { title = title ++ " - rizzmi"
            , body =
                List.map (H.map (\msg -> GotHomeMsg msg)) body
            }

        Login login ->
            let
                { title, body } =
                    Login.view login
            in
            { title = title ++ " - rizzmi"
            , body =
                List.map (H.map (\msg -> GotLoginMsg msg)) body
            }

        Signup signup ->
            let
                { title, body } =
                    Signup.view signup
            in
            { title = title ++ " - rizzmi"
            , body =
                List.map (H.map (\msg -> GotSignupMsg msg)) body
            }

        Welcome welcome ->
            let
                { title, body } =
                    Welcome.view welcome
            in
            { title = title ++ " - rizzmi"
            , body =
                List.map (H.map (\msg -> GotWelcomeMsg msg)) body
            }

        NotFound session ->
            let
                { title, body } =
                    NotFound.view <| NotFound.init session
            in
            { title = title ++ " - rizzmi"
            , body =
                List.map (H.map (\_ -> DoNothing)) body
            }

        NotImplemented _ ->
            { title = "rizzmi"
            , body =
                [ H.h1 [] [ H.text "Page Not Implemented" ]
                , H.p [] [ H.text "Go back ", H.a [ Route.href Route.Home ] [ H.text "Home" ] ]
                ]
            }

        Redirect _ ->
            { title = "Loading"
            , body =
                [ H.h3 [] [ H.text "Loading..." ] ]
            }

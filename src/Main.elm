module Main exposing (..)

import Api exposing (Cred)
import Browser
import Browser.Navigation as Nav
import Html as H
import Json.Decode as Decode exposing (Value)
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


type Model
    = Redirect Session
    | Home Home.Model
    | Login Login.Model
    | Signup Signup.Model
    | Welcome Welcome.Model
    | MyProfile MyProfile.Model
    | NotFound NotFound.Model
    | NotImplemented Session


init : Maybe Viewer -> Url -> Nav.Key -> ( Model, Cmd Msg )
init maybeViewer url navKey =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Session.fromViewer navKey maybeViewer))



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

        MyProfile profile ->
            MyProfile.toSession profile

        NotFound notFound ->
            NotFound.toSession notFound

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
            NotFound.init session
                |> updateWith NotFound GotNotFoundMsg

        Just Route.Root ->
            ( model, Route.replaceUrl (Session.navKey session) Route.Home )

        Just Route.Home ->
            Home.init session
                |> updateWith Home GotHomeMsg

        Just Route.Login ->
            Login.init session
                |> updateWith Login GotLoginMsg

        Just Route.Signup ->
            Signup.init session
                |> updateWith Signup GotSignupMsg

        Just Route.Logout ->
            ( model, Api.logout )

        Just Route.Welcome ->
            Welcome.init session
                |> updateWith Welcome GotWelcomeMsg

        Just Route.MyProfile ->
            MyProfile.init session
                |> updateWith MyProfile GotMyProfileMsg


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

        ( GotMyProfileMsg subMsg, MyProfile profile ) ->
            MyProfile.update subMsg profile
                |> updateWith MyProfile GotMyProfileMsg

        ( GotNotFoundMsg subMsg, NotFound notFound ) ->
            NotFound.update subMsg notFound
                |> updateWith NotFound GotNotFoundMsg

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
subscriptions model =
    case model of
        Redirect _ ->
            Session.changes GotSession (Session.navKey (toSession model))

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

        NotImplemented _ ->
            Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        viewer =
            Session.viewer <| toSession model

        viewPage toMsg page content =
            let
                { title, body } =
                    Page.view viewer page content
            in
            { title = title
            , body = List.map (H.map toMsg) body
            }
    in
    case model of
        Home home ->
            --let
            --    { title, body } =
            --        Home.view home
            --in
            --{ title = "rizzmi/" ++ title
            --, body = List.map (H.map (\msg -> GotHomeMsg msg)) body
            --}
            viewPage GotHomeMsg Page.Home (Home.view home)

        Login login ->
            --let
            --    { title, body } =
            --        Login.view login
            --in
            --{ title = "rizzmi/" ++ title
            --, body = List.map (H.map (\msg -> GotLoginMsg msg)) body
            --}
            viewPage GotLoginMsg Page.Login (Login.view login)

        Signup signup ->
            --let
            --    { title, body } =
            --        Signup.view signup
            --in
            --{ title = "rizzmi/" ++ title
            --, body = List.map (H.map (\msg -> GotSignupMsg msg)) body
            --}
            viewPage GotSignupMsg Page.Signup (Signup.view signup)

        Welcome welcome ->
            --let
            --    { title, body } =
            --        Welcome.view welcome
            --in
            --{ title = "rizzmi/" ++ title
            --, body = List.map (H.map (\msg -> GotWelcomeMsg msg)) body
            --}
            viewPage GotWelcomeMsg Page.Welcome (Welcome.view welcome)

        MyProfile profile ->
            --let
            --    { title, body } =
            --        MyProfile.view profile
            --in
            --{ title = "rizzmi/" ++ title
            --, body = List.map (H.map (\msg -> GotMyProfileMsg msg)) body
            --}
            viewPage GotMyProfileMsg Page.MyProfile (MyProfile.view profile)

        NotFound notFound ->
            --let
            --    { title, body } =
            --        NotFound.view notFound
            --in
            --{ title = "rizzmi/" ++ title
            --, body = List.map (H.map (\_ -> DoNothing)) body
            --}
            viewPage GotNotFoundMsg Page.NotFound (NotFound.view notFound)

        NotImplemented _ ->
            { title = "rizzmi"
            , body =
                [ H.h1 [] [ H.text "Page Not Implemented" ]
                , H.p [] [ H.text "Go back ", H.a [ Route.href Route.Home ] [ H.text "Home" ] ]
                ]
            }

        Redirect _ ->
            { title = "rizzmi/loading"
            , body = [ H.h3 [] [ H.text "Loading..." ] ]
            }

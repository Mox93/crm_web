module Page.Welcome exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Brand
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Layout
import Route
import Session exposing (Session)
import User
import Viewer



-- MODEL


type alias NewCompanyForm =
    { name : String
    , email : String
    , phoneNumber : String
    }


type alias JoinCompanyForm =
    { name : String }


type Model
    = Welcome Session
    | CreateCompany Session NewCompanyForm
    | JoinCompany Session JoinCompanyForm


init : Session -> ( Model, Cmd msg )
init session =
    ( Welcome session, Cmd.none )



-- UPDATE


type Msg
    = ShowWelcome
    | ShowCreateForm
    | ShowJoinForm
    | SubmitCreateForm
    | SubmitJoinForm
    | ChangeName String
    | ChangeEmail String
    | ChangePhoneNumber String
    | SearchForCompany String
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        session =
            toSession model
    in
    case msg of
        ShowCreateForm ->
            let
                form =
                    NewCompanyForm "" "" ""
            in
            ( CreateCompany session form, Cmd.none )

        ShowJoinForm ->
            let
                form =
                    JoinCompanyForm ""
            in
            ( JoinCompany session form, Cmd.none )

        SubmitCreateForm ->
            ( model, Cmd.none )

        SubmitJoinForm ->
            ( model, Cmd.none )

        ChangeName name ->
            case model of
                CreateCompany _ form ->
                    ( CreateCompany session { form | name = name }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ChangeEmail email ->
            case model of
                CreateCompany _ form ->
                    ( CreateCompany session { form | email = email }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ChangePhoneNumber phoneNumber ->
            case model of
                CreateCompany _ form ->
                    ( CreateCompany session { form | phoneNumber = phoneNumber }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SearchForCompany name ->
            case model of
                JoinCompany _ form ->
                    ( JoinCompany session { form | name = name }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        ShowWelcome ->
            ( Welcome session, Cmd.none )

        GotSession session_ ->
            ( Welcome session_, Cmd.none )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey <| toSession model)



-- VIEW


view : Model -> { title : String, body : Element Msg }
view model =
    { title = "Welcome"
    , body =
        case model of
            Welcome session ->
                viewWelcomeCard session

            CreateCompany session form ->
                viewCreateForm form

            JoinCompany session form ->
                viewJoinForm form
    }


viewWelcomeCard : Session -> Element Msg
viewWelcomeCard session =
    Layout.card <|
        column
            [ spacing <| Brand.scaled 1
            ]
            [ Layout.headerText <|
                case Session.viewer session of
                    Just viewer ->
                        "Welcome "
                            ++ (User.fullName <| Viewer.info viewer)
                            ++ "!"

                    Nothing ->
                        "Welcome!"
            , row
                [ spacing <| Brand.scaled 1 ]
                [ viewCreateBtn
                , viewJoinBtn
                ]
            ]


viewCreateBtn : Element Msg
viewCreateBtn =
    Input.button
        [ width (px <| Brand.scaled 15)
        , height (px <| Brand.scaled 13)
        , Font.bold
        , Font.size <| Brand.scaled 3
        , Font.color Brand.primaryTextColorDBg
        , Background.color Brand.primaryColor -- <| rgb255 225 225 76
        , Border.rounded <| Brand.scaled -3
        ]
        { onPress = Just ShowCreateForm
        , label =
            el
                [ centerX
                , centerY
                ]
                (text "Create a company")
        }


viewJoinBtn : Element Msg
viewJoinBtn =
    Input.button
        [ width (px <| Brand.scaled 15)
        , height (px <| Brand.scaled 13)
        , Font.bold
        , Font.size <| Brand.scaled 3
        , Font.color Brand.primaryTextColorDBg
        , Background.color Brand.primaryColor -- <| rgb255 39 52 84
        , Border.rounded <| Brand.scaled -3
        ]
        { onPress = Just ShowJoinForm
        , label =
            el
                [ centerX
                , centerY
                ]
                (text "Join a company")
        }


viewCreateForm : NewCompanyForm -> Element Msg
viewCreateForm form =
    Layout.card <|
        column
            [ spacing <| Brand.scaled 1
            ]
            [ row
                [ width fill ]
                [ el [ alignLeft ] viewBackBtn
                , el [ centerX ] <| Layout.headerText "Create Company"
                , el [ width (px 24), height (px 24) ] Element.none
                ]
            , viewName form.name
            , viewEmail form.email
            , viewPhoneNumber form.phoneNumber
            , submitBan <| text "Create"
            ]


viewJoinForm : JoinCompanyForm -> Element Msg
viewJoinForm form =
    Layout.card <|
        column
            [ spacing <| Brand.scaled 1
            ]
            [ row
                [ width fill ]
                [ el [ alignLeft ] viewBackBtn
                , el [ centerX ] <| Layout.headerText "Join Company"
                , el [ width (px 24), height (px 24) ] Element.none
                ]
            , viewName form.name
            , submitBan <| text "Join"
            ]


viewBackBtn : Element Msg
viewBackBtn =
    Input.button
        []
        { onPress = Just ShowWelcome
        , label =
            image []
                { src = "web/static/assets/arrow_back_ios-24px.svg"
                , description = "Back"
                }
        }


viewName : String -> Element Msg
viewName name =
    Layout.textInput
        [ Input.text
            [ paddingXY 0 <| Brand.scaled -1
            , Border.width 0
            , Background.color Brand.transparent
            , focused []
            ]
            { onChange = ChangeName
            , text = name
            , placeholder =
                Just <|
                    Input.placeholder []
                        (el
                            [ Font.color Brand.subtleTextColor ]
                            (text "Company Name")
                        )
            , label = Input.labelHidden "Company Name"
            }
        ]
        True


viewSearch : String -> Element Msg
viewSearch name =
    Layout.textInput
        [ Input.text
            [ paddingXY 0 <| Brand.scaled -1
            , Border.width 0
            , Background.color Brand.transparent
            , focused []
            ]
            { onChange = SearchForCompany
            , text = name
            , placeholder =
                Just <|
                    Input.placeholder []
                        (el
                            [ Font.color Brand.subtleTextColor ]
                            (text "Company Name")
                        )
            , label = Input.labelHidden "Company Name"
            }
        ]
        True


viewEmail : String -> Element Msg
viewEmail email =
    Layout.textInput
        [ Input.email
            [ paddingXY 0 <| Brand.scaled -1
            , Border.width 0
            , Background.color Brand.transparent
            , focused []
            ]
            { onChange = ChangeEmail
            , text = email
            , placeholder =
                Just <|
                    Input.placeholder []
                        (el
                            [ Font.color Brand.subtleTextColor ]
                            (text "Company Email")
                        )
            , label = Input.labelHidden "Company Email"
            }
        ]
        True


submitBan : Element Msg -> Element Msg
submitBan content =
    el
        [ width fill
        , height (px <| Brand.scaled 6)
        , Font.bold
        , Font.size <| Brand.scaled 3
        , Font.color Brand.primaryTextColorDBg
        , Background.color Brand.primaryColor
        , Border.rounded <| Brand.scaled -3
        ]
    <|
        el
            [ centerX
            , centerY
            ]
            content


viewPhoneNumber : String -> Element Msg
viewPhoneNumber phone =
    Layout.textInput
        [ Input.text
            [ paddingXY 0 <| Brand.scaled -1
            , Border.width 0
            , Background.color Brand.transparent
            , focused []
            ]
            { onChange = ChangePhoneNumber
            , text = phone
            , placeholder =
                Just <|
                    Input.placeholder []
                        (el
                            [ Font.color Brand.subtleTextColor ]
                            (text "Company Phone number")
                        )
            , label = Input.labelHidden "Company Phone Number"
            }
        ]
        True



-- EXPORT


toSession : Model -> Session
toSession model =
    case model of
        Welcome session ->
            session

        CreateCompany session _ ->
            session

        JoinCompany session _ ->
            session

module Page.Login exposing (Model, Msg, init, toSession, update, view)

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



-- MODEL


type alias Form =
    { email : String
    , password : String
    , showPassword : Bool
    , rememberMe : Bool
    }


type alias Model =
    { session : Session
    , form : Form
    }


init : Session -> ( Model, Cmd msg )
init session =
    ( Model session (Form "" "" False False), Cmd.none )



-- UPDATE


type Msg
    = ChangeEmail String
    | ChangePassword String
    | ToggleShowPassword Bool
    | ToggleRememberMe Bool
    | SubmitForm Form


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeEmail email ->
            updateForm (\form -> { form | email = email }) model

        ChangePassword password ->
            updateForm (\form -> { form | password = password }) model

        ToggleRememberMe rememberMe ->
            updateForm (\form -> { form | rememberMe = rememberMe }) model

        ToggleShowPassword show ->
            updateForm (\form -> { form | showPassword = show }) model

        SubmitForm form ->
            ( model, Cmd.none )


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Login"
    , body =
        [ layout
            [ inFront <| Layout.appBar appBarContent
            , Brand.defaultBodyPadding
            , Background.color Brand.canvasColor
            , Brand.defaultFont
            ]
          <|
            viewForm model.form
        ]
    }


viewForm : Form -> Element Msg
viewForm form =
    Layout.card <|
        column
            [ spacing <| Brand.scaled 1
            ]
            [ Layout.viewHeader "Login"
            , row
                [ centerX
                , Font.color Brand.subtleTextColor
                , Font.size <| Brand.scaled 1
                ]
                [ text "New to rizzmi? "
                , link
                    [ Font.color Brand.primaryColor
                    , Font.semiBold
                    ]
                    { url = Route.toString Route.Signup
                    , label = text "Join us now"
                    }
                ]

            -- Email
            , viewEmail form.email

            -- Password
            , viewPassword form.password form.showPassword

            -- Remember Me
            , viewRememberMe form.rememberMe

            -- Login
            , viewLoginBtn form
            ]


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
                            (text "Email")
                        )
            , label = Input.labelHidden "Email"
            }
        ]


viewPassword : String -> Bool -> Element Msg
viewPassword password showPassword =
    Layout.textInput
        [ Input.currentPassword
            [ paddingXY 0 <| Brand.scaled -1
            , Border.width 0
            , Background.color Brand.transparent
            , focused []
            ]
            { onChange = ChangePassword
            , text = password
            , placeholder =
                Just <|
                    Input.placeholder []
                        (el
                            [ Font.color Brand.subtleTextColor ]
                            (text "Password")
                        )
            , label =
                Input.labelHidden "Password"
            , show = showPassword
            }
        , Input.checkbox [ width shrink ]
            { onChange = ToggleShowPassword
            , icon = Brand.eye
            , checked = showPassword
            , label =
                Input.labelHidden "Show Password"
            }
        ]


viewRememberMe : Bool -> Element Msg
viewRememberMe rememberMe =
    row
        [ spacing <| Brand.scaled -3 ]
        [ Input.checkbox [ width shrink ]
            { onChange = ToggleRememberMe
            , icon = Brand.radio
            , checked = rememberMe
            , label = Input.labelHidden "Remember Me"
            }
        , el
            [ Font.color Brand.subtleTextColor
            , Font.size <| Brand.scaled 1
            , alignTop
            ]
            (text "Remember Me")
        ]


viewLoginBtn : Form -> Element Msg
viewLoginBtn form =
    Input.button
        [ centerX
        , width fill
        , height (px <| Brand.scaled 6)
        , Font.bold
        , Font.size <| Brand.scaled 3
        , Font.color Brand.primaryTextColorDBg
        , Background.color Brand.primaryColor
        , Border.rounded <| Brand.scaled -3
        ]
        { onPress = Just (SubmitForm form)
        , label = el [ centerX, centerY ] (text "Login")
        }


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
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session

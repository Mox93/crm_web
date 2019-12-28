module Page.Signup exposing (Model, Msg, init, toSession, update, view)

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
    { firstName : String
    , lastName : String
    , email : String
    , phoneNumber : String
    , password : String
    , showPassword : Bool
    , agree : Bool
    }


type alias Model =
    { session : Session
    , form : Form
    }


init : Session -> ( Model, Cmd msg )
init session =
    ( Model session (Form "" "" "" "" "" False False), Cmd.none )



-- UPDATE


type Msg
    = ChangeFirstName String
    | ChangeLastName String
    | ChangeEmail String
    | ChangePhoneNumber String
    | ChangePassword String
    | ToggleShowPassword Bool
    | ToggleAgree Bool
    | SubmitForm Form


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeFirstName firstName ->
            updateForm (\form -> { form | firstName = firstName }) model

        ChangeLastName lastName ->
            updateForm (\form -> { form | lastName = lastName }) model

        ChangeEmail email ->
            updateForm (\form -> { form | email = email }) model

        ChangePhoneNumber phoneNumber ->
            updateForm (\form -> { form | phoneNumber = phoneNumber }) model

        ChangePassword password ->
            updateForm (\form -> { form | password = password }) model

        ToggleShowPassword show ->
            updateForm (\form -> { form | showPassword = show }) model

        ToggleAgree agree ->
            updateForm (\form -> { form | agree = agree }) model

        SubmitForm form ->
            ( model, Cmd.none )


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Sign up"
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
            [ Layout.viewHeader "Sign up"
            , row
                [ centerX
                , Font.color Brand.subtleTextColor
                , Font.size <| Brand.scaled 1
                ]
                [ text "A rizzmi member? "
                , link
                    [ Font.color Brand.primaryColor
                    , Font.semiBold
                    ]
                    { url = Route.toString Route.Login
                    , label = text "Come on in"
                    }
                ]

            -- Name
            , viewName form.firstName form.lastName

            -- Email
            , viewEmail form.email

            -- Phone Number
            , viewPhoneNumber form.phoneNumber

            -- Password
            , viewPassword form.password form.showPassword

            -- Terms & Conditions
            , viewAgreement form.agree

            -- Login
            , viewSignupBtn form
            ]


viewName : String -> String -> Element Msg
viewName firstName lastName =
    row
        [ spacing <| Brand.scaled 1
        , width (px <| Brand.scaled 15)
        ]
        [ Input.text
            [ paddingXY 0 <| Brand.scaled -1
            , Background.color Brand.transparent
            , Brand.bottomBorder 2
            , Border.color Brand.secondaryColorMuted
            , Border.rounded 0
            , focused [ Border.color Brand.primaryColor ]
            ]
            { onChange = ChangeFirstName
            , text = firstName
            , placeholder =
                Just <|
                    Input.placeholder []
                        (el
                            [ Font.color Brand.subtleTextColor ]
                            (text "First name")
                        )
            , label = Input.labelHidden "First Name"
            }
        , Input.text
            [ paddingXY 0 <| Brand.scaled -1
            , Background.color Brand.transparent
            , Brand.bottomBorder 2
            , Border.color Brand.secondaryColorMuted
            , Border.rounded 0
            , focused [ Border.color Brand.primaryColor ]
            ]
            { onChange = ChangeLastName
            , text = lastName
            , placeholder =
                Just <|
                    Input.placeholder []
                        (el
                            [ Font.color Brand.subtleTextColor ]
                            (text "Last name")
                        )
            , label = Input.labelHidden "Last Name"
            }
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
                            (text "Phone number")
                        )
            , label = Input.labelHidden "Phone Number"
            }
        ]


viewPassword : String -> Bool -> Element Msg
viewPassword password showPassword =
    Layout.textInput
        [ Input.newPassword
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


viewAgreement : Bool -> Element Msg
viewAgreement rememberMe =
    row
        [ spacing <| Brand.scaled -3 ]
        [ Input.checkbox [ width shrink ]
            { onChange = ToggleAgree
            , icon = Brand.radio
            , checked = rememberMe
            , label = Input.labelHidden "Terms & Conditions"
            }
        , row
            [ Font.color Brand.subtleTextColor
            , Font.size <| Brand.scaled 1
            , alignTop
            ]
            [ text "I agree to the "
            , link
                [ Font.color Brand.primaryColor
                , Font.semiBold
                ]
                { url = "#"
                , label = text "Terms & Conditions"
                }
            , text "."
            ]
        ]


viewSignupBtn : Form -> Element Msg
viewSignupBtn form =
    row
        [ centerX
        , spacing <| Brand.scaled 2
        , width fill
        ]
        [ link
            [ width fill
            ]
            { url = Route.toString Route.Welcome
            , label =
                signupBtn <| text "Sign up"
            }
        ]


signupBtn : Element Msg -> Element Msg
signupBtn content =
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



{-
   Input.button
       [ centerX
       , centerY
       , width fill
       , height (px <| Brand.scaled 6)
       , Font.bold
       , Font.size <| Brand.scaled 3
       , Font.color Brand.primaryTextColorDBg
       , Background.color Brand.primaryColor
       , Border.rounded <| Brand.scaled -3
       ]
       { onPress = Just (SubmitForm form)
       , label = el [ centerX, centerY ] (text "Sign up")
       }
-}


appBarContent : Element msg
appBarContent =
    row
        [ alignRight
        , spacing <| Brand.scaled 2
        ]
        [ link []
            { url = Route.toString Route.Login
            , label = Layout.loginBtn
            }
        ]



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session

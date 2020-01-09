module Page.Signup exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api
import Brand
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import ErrorHandling exposing (httpErrorToString)
import Graphql.Http
import Layout
import RemoteData exposing (RemoteData(..))
import Route
import Session exposing (Session)
import Viewer exposing (Viewer)



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


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


type alias Model =
    { session : Session
    , form : Form
    , problems : List Problem
    }


init : Session -> ( Model, Cmd msg )
init session =
    ( Model session (Form "" "" "" "" "" False False) [], Cmd.none )



-- UPDATE


type Msg
    = ChangeFirstName String
    | ChangeLastName String
    | ChangeEmail String
    | ChangePhoneNumber String
    | ChangePassword String
    | ToggleShowPassword Bool
    | ToggleAgree Bool
    | SubmitForm
    | GotResponse (RemoteData (Graphql.Http.Error Viewer) Viewer)
    | GotSession Session


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

        SubmitForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , Api.signup ((\(Trimmed form) -> form) validForm)
                        GotResponse
                        Viewer.selectionSet
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        GotResponse response ->
            case response of
                Success viewer ->
                    ( model
                    , Cmd.batch
                        [ Viewer.store viewer
                        , Route.replaceUrl (Session.navKey model.session) Route.Welcome
                        ]
                    )

                Failure err ->
                    case err of
                        Graphql.Http.GraphqlError _ gQLErr ->
                            ( { model | problems = List.map (\e -> ServerError e.message) gQLErr }
                            , Cmd.none
                            )

                        Graphql.Http.HttpError httpErr ->
                            ( { model | problems = [ ServerError <| httpErrorToString httpErr ] }
                            , Cmd.none
                            )

                _ ->
                    ( model, Cmd.none )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Welcome
            )


updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- FORM


{-| Marks that we've trimmed the form's fields, so we don't accidentally send
it to the server without having trimmed it!
-}
type TrimmedForm
    = Trimmed Form


{-| When adding a variant here, add it to `fieldsToValidate` too!
-}
type ValidatedField
    = FirstName
    | LastName
    | Email
    | PhoneNumber
    | Password


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ FirstName
    , LastName
    , Email
    , PhoneNumber
    , Password
    ]


{-| Trim the form and validate its fields. If there are problems, report them!
-}
validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            FirstName ->
                if String.isEmpty form.firstName then
                    [ "first name can't be blank." ]

                else
                    []

            LastName ->
                if String.isEmpty form.lastName then
                    [ "last name can't be blank." ]

                else
                    []

            Email ->
                if String.isEmpty form.email then
                    [ "email can't be blank." ]

                else
                    []

            PhoneNumber ->
                if String.isEmpty form.phoneNumber then
                    [ "phone number can't be blank." ]

                else
                    []

            Password ->
                if String.isEmpty form.password then
                    [ "password can't be blank." ]

                else
                    []


{-| Don't trim while the user is typing! That would be super annoying.
Instead, trim only on submit.
-}
trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { form
            | firstName = String.trim form.firstName
            , lastName = String.trim form.lastName
            , email = String.trim form.email
            , phoneNumber = String.trim form.phoneNumber
            , password = String.trim form.password
        }


problemToString : Problem -> String
problemToString problem =
    case problem of
        InvalidEntry _ str ->
            str

        ServerError str ->
            str


isServerError : Problem -> Bool
isServerError problem =
    case problem of
        ServerError _ ->
            True

        _ ->
            False


isFieldError : ValidatedField -> Problem -> Bool
isFieldError field problem =
    case problem of
        InvalidEntry f _ ->
            if f == field then
                True

            else
                False

        ServerError _ ->
            False



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
            viewForm model.form model.problems
        ]
    }


viewForm : Form -> List Problem -> Element Msg
viewForm form problems =
    Layout.card <|
        column
            [ spacing <| Brand.scaled 1
            ]
            [ Layout.viewHeader "Sign up"
            , paragraph
                [ centerX
                , Font.color Brand.subtleTextColor
                , Font.size <| Brand.scaled 1
                , width shrink
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

            -- Errors
            , viewErrors (List.filter isServerError problems)

            -- Name
            , viewName form.firstName form.lastName problems

            -- Email
            , viewEmail form.email
                (List.filter (isFieldError Email) problems)

            -- Phone Number
            , viewPhoneNumber form.phoneNumber
                (List.filter (isFieldError PhoneNumber) problems)

            -- Password
            , viewPassword form.password
                form.showPassword
                (List.filter (isFieldError Password) problems)

            -- Terms & Conditions
            , viewAgreement form.agree

            -- Signup
            , viewSignupBtn
            ]


viewErrors : List Problem -> Element msg
viewErrors problems =
    column
        [ Font.color Brand.warningColor
        , Font.size <| Brand.scaled -1
        , width (maximum (Brand.scaled 15) fill)
        ]
    <|
        List.map
            (\err ->
                paragraph
                    []
                    [ text <| "* " ++ problemToString err ]
            )
            problems


viewName : String -> String -> List Problem -> Element Msg
viewName firstName lastName problems =
    let
        fnp =
            List.filter (isFieldError FirstName) problems

        fnOk =
            List.isEmpty fnp

        lnp =
            List.filter (isFieldError LastName) problems

        lnOk =
            List.isEmpty lnp
    in
    row
        [ spacing <| Brand.scaled 1
        , width (px <| Brand.scaled 15)
        ]
        [ Layout.withErrors
            (Input.text
                [ paddingXY 0 <| Brand.scaled -1
                , Background.color Brand.transparent
                , Brand.underlined 2
                , case List.isEmpty fnp of
                    True ->
                        Border.color Brand.secondaryColorMuted

                    False ->
                        Border.color Brand.warningColor
                , Border.rounded 0
                , focused [ Border.color Brand.primaryColor ]
                ]
                { onChange = ChangeFirstName
                , text = firstName
                , placeholder =
                    Just <|
                        Input.placeholder []
                            (el
                                [ case fnOk of
                                    True ->
                                        Font.color Brand.subtleTextColor

                                    False ->
                                        Font.color Brand.warningColor
                                ]
                                (text "First name")
                            )
                , label = Input.labelHidden "First Name"
                }
            )
            (viewErrors fnp)
        , Layout.withErrors
            (Input.text
                [ paddingXY 0 <| Brand.scaled -1
                , Background.color Brand.transparent
                , Brand.underlined 2
                , case List.isEmpty lnp of
                    True ->
                        Border.color Brand.secondaryColorMuted

                    False ->
                        Border.color Brand.warningColor
                , Border.rounded 0
                , focused [ Border.color Brand.primaryColor ]
                ]
                { onChange = ChangeLastName
                , text = lastName
                , placeholder =
                    Just <|
                        Input.placeholder []
                            (el
                                [ case lnOk of
                                    True ->
                                        Font.color Brand.subtleTextColor

                                    False ->
                                        Font.color Brand.warningColor
                                ]
                                (text "Last name")
                            )
                , label = Input.labelHidden "Last Name"
                }
            )
            (viewErrors lnp)
        ]


viewEmail : String -> List Problem -> Element Msg
viewEmail email problems =
    let
        ok =
            List.isEmpty problems
    in
    Layout.withErrors
        (Layout.textInput
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
                                [ case ok of
                                    True ->
                                        Font.color Brand.subtleTextColor

                                    False ->
                                        Font.color Brand.warningColor
                                ]
                                (text "Email")
                            )
                , label = Input.labelHidden "Email"
                }
            ]
            (List.isEmpty problems)
        )
        (viewErrors problems)


viewPhoneNumber : String -> List Problem -> Element Msg
viewPhoneNumber phone problems =
    let
        ok =
            List.isEmpty problems
    in
    Layout.withErrors
        (Layout.textInput
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
                                [ case ok of
                                    True ->
                                        Font.color Brand.subtleTextColor

                                    False ->
                                        Font.color Brand.warningColor
                                ]
                                (text "Phone number")
                            )
                , label = Input.labelHidden "Phone Number"
                }
            ]
            (List.isEmpty problems)
        )
        (viewErrors problems)


viewPassword : String -> Bool -> List Problem -> Element Msg
viewPassword password showPassword problems =
    let
        ok =
            List.isEmpty problems
    in
    Layout.withErrors
        (Layout.textInput
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
                                [ case ok of
                                    True ->
                                        Font.color Brand.subtleTextColor

                                    False ->
                                        Font.color Brand.warningColor
                                ]
                                (text "Password")
                            )
                , label =
                    Input.labelHidden "Password"
                , show = showPassword
                }
            , Input.checkbox [ width shrink ]
                { onChange = ToggleShowPassword
                , icon = Layout.eye
                , checked = showPassword
                , label =
                    Input.labelHidden "Show Password"
                }
            ]
            (List.isEmpty problems)
        )
        (viewErrors problems)


viewAgreement : Bool -> Element Msg
viewAgreement rememberMe =
    row
        [ spacing <| Brand.scaled -3 ]
        [ Input.checkbox [ width shrink ]
            { onChange = ToggleAgree
            , icon = Layout.radio
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


viewSignupBtn : Element Msg
viewSignupBtn =
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
        { onPress = Just SubmitForm
        , label = el [ centerX, centerY ] (text "Sign up")
        }



{-
   viewSignupBtn : Element Msg
   viewSignupBtn =
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

module Page.Signup exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api
import Brand
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import ErrorHandling exposing (..)
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
    , agreement : Bool
    }


type alias Model =
    { session : Session
    , form : Form
    , problems : List (Problem ValidatedField)
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
    | ToggleAgreement Bool
    | SubmitForm
    | GotResponse (RemoteData (Graphql.Http.Error Viewer) Viewer)
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeFirstName firstName ->
            ( { model | problems = removeFieldError FirstName model.problems }
                |> updateForm (\form -> { form | firstName = firstName })
            , Cmd.none
            )

        ChangeLastName lastName ->
            ( { model | problems = removeFieldError LastName model.problems }
                |> updateForm (\form -> { form | lastName = lastName })
            , Cmd.none
            )

        ChangeEmail email ->
            ( { model | problems = removeFieldError Email model.problems }
                |> updateForm (\form -> { form | email = email })
            , Cmd.none
            )

        ChangePhoneNumber phoneNumber ->
            ( { model | problems = removeFieldError PhoneNumber model.problems }
                |> updateForm (\form -> { form | phoneNumber = phoneNumber })
            , Cmd.none
            )

        ChangePassword password ->
            ( { model | problems = removeFieldError Password model.problems }
                |> updateForm (\form -> { form | password = password })
            , Cmd.none
            )

        ToggleShowPassword show ->
            ( updateForm (\form -> { form | showPassword = show }) model
            , Cmd.none
            )

        ToggleAgreement agree ->
            ( { model | problems = removeFieldError Agreement model.problems }
                |> updateForm (\form -> { form | agreement = agree })
            , Cmd.none
            )

        SubmitForm ->
            let
                validation =
                    validate
                        (trimFields model.form)
                        fieldsToValidate
                        validateField
            in
            case validation of
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
                    , Viewer.store viewer
                    )

                Failure err ->
                    case err of
                        Graphql.Http.GraphqlError _ gQLErr ->
                            ( { model
                                | problems =
                                    List.concatMap
                                        (graphqlErrorToProblem fieldsToValidate)
                                        gQLErr
                              }
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
            , Route.replaceUrl (Session.navKey session) Route.Signup
            )


updateForm : (Form -> Form) -> Model -> Model
updateForm transform model =
    { model | form = transform model.form }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- FORM


{-| When adding a variant here, add it to `fieldsToValidate` too!
-}
type ValidatedField
    = FirstName
    | LastName
    | Email
    | PhoneNumber
    | Password
    | Agreement


fieldsToValidate : List ( String, ValidatedField )
fieldsToValidate =
    [ ( "firstName", FirstName )
    , ( "lastName", LastName )
    , ( "email", Email )
    , ( "phoneNumber", PhoneNumber )
    , ( "password", Password )
    , ( "agreement", Agreement )
    ]


validateField : TrimmedForm Form -> ( String, ValidatedField ) -> List (Problem ValidatedField)
validateField (Trimmed form) ( _, field ) =
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

            Agreement ->
                if form.agreement == False then
                    [ "You must agree to the terms and conditions." ]

                else
                    []


{-| Don't trim while the user is typing! That would be super annoying.
Instead, trim only on submit.
-}
trimFields : Form -> TrimmedForm Form
trimFields form =
    Trimmed
        { form
            | firstName = String.trim form.firstName
            , lastName = String.trim form.lastName
            , email = String.trim form.email
            , phoneNumber = String.trim form.phoneNumber
            , password = String.trim form.password
        }



-- VIEW


view : Model -> { title : String, body : Element Msg }
view model =
    { title = "Sign Up"
    , body =
        viewForm model.form model.problems
    }


viewForm : Form -> List (Problem ValidatedField) -> Element Msg
viewForm form problems =
    Layout.card
        []
    <|
        column
            [ spacing <| Brand.scaled 1
            ]
            [ Layout.headerText "Sign up"
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
            , Layout.errNote (List.filter isServerError problems)

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
            , viewAgreement form.agreement
                (List.filter (isFieldError Agreement) problems)

            -- Signup
            , viewSignupBtn
            ]


viewName : String -> String -> List (Problem ValidatedField) -> Element Msg
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
                , Border.color <|
                    if fnOk then
                        Brand.secondaryColorMuted

                    else
                        Brand.warningColor
                , Border.rounded 0
                , focused [ Border.color Brand.primaryColor ]
                ]
                { onChange = ChangeFirstName
                , text = firstName
                , placeholder =
                    Just <|
                        Input.placeholder
                            [ Font.color <|
                                if fnOk then
                                    Brand.subtleTextColor

                                else
                                    Brand.warningColor
                            ]
                            (text "First name")
                , label = Input.labelHidden "First Name"
                }
            )
            fnp
        , Layout.withErrors
            (Input.text
                [ paddingXY 0 <| Brand.scaled -1
                , Background.color Brand.transparent
                , Brand.underlined 2
                , Border.color <|
                    if lnOk then
                        Brand.secondaryColorMuted

                    else
                        Brand.warningColor
                , Border.rounded 0
                , focused [ Border.color Brand.primaryColor ]
                ]
                { onChange = ChangeLastName
                , text = lastName
                , placeholder =
                    Just <|
                        Input.placeholder
                            [ Font.color <|
                                if lnOk then
                                    Brand.subtleTextColor

                                else
                                    Brand.warningColor
                            ]
                            (text "Last name")
                , label = Input.labelHidden "Last Name"
                }
            )
            lnp
        ]


viewEmail : String -> List (Problem ValidatedField) -> Element Msg
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
                        Input.placeholder
                            [ Font.color <|
                                if ok then
                                    Brand.subtleTextColor

                                else
                                    Brand.warningColor
                            ]
                            (text "Email")
                , label = Input.labelHidden "Email"
                }
            ]
            ok
        )
        problems


viewPhoneNumber : String -> List (Problem ValidatedField) -> Element Msg
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
                        Input.placeholder
                            [ Font.color <|
                                if ok then
                                    Brand.subtleTextColor

                                else
                                    Brand.warningColor
                            ]
                            (text "Phone number")
                , label = Input.labelHidden "Phone Number"
                }
            ]
            ok
        )
        problems


viewPassword : String -> Bool -> List (Problem ValidatedField) -> Element Msg
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
                        Input.placeholder
                            [ Font.color <|
                                if ok then
                                    Brand.subtleTextColor

                                else
                                    Brand.warningColor
                            ]
                            (text "Password")
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
            ok
        )
        problems


viewAgreement : Bool -> List (Problem ValidatedField) -> Element Msg
viewAgreement agree problems =
    let
        ok =
            List.isEmpty problems
    in
    Layout.withErrors
        (row
            [ spacing <| Brand.scaled -3 ]
            [ Input.checkbox [ width shrink ]
                { onChange = ToggleAgreement
                , icon = Layout.radio ok
                , checked = agree
                , label =
                    Input.labelRight [ centerY ] <|
                        row
                            [ Font.color <|
                                if ok then
                                    Brand.subtleTextColor

                                else
                                    Brand.warningColor
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
                            ]
                }
            ]
        )
        problems


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



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session

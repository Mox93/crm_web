module Page.Login exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Cred)
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
    { email : String
    , password : String
    , showPassword : Bool
    , rememberMe : Bool
    }


type alias Model =
    { session : Session
    , form : Form
    , problems : List (Problem ValidatedField)
    }


init : Session -> ( Model, Cmd msg )
init session =
    ( Model session (Form "" "" False False) [], Cmd.none )



-- UPDATE


type Msg
    = ChangeEmail String
    | ChangePassword String
    | ToggleShowPassword Bool
    | ToggleRememberMe Bool
    | SubmitForm
    | GotResponse (RemoteData (Graphql.Http.Error Viewer) Viewer)
    | GotSession Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeEmail email ->
            ( { model | problems = removeFieldError Email model.problems }
                |> updateForm (\form -> { form | email = email })
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

        ToggleRememberMe rememberMe ->
            ( updateForm (\form -> { form | rememberMe = rememberMe }) model
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
                    , Api.login ((\(Trimmed form) -> form) validForm)
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
            , Route.replaceUrl (Session.navKey session) Route.Login
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
    = Email
    | Password


fieldsToValidate : List ( String, ValidatedField )
fieldsToValidate =
    [ ( "email", Email )
    , ( "password", Password )
    ]


validateField : TrimmedForm Form -> ( String, ValidatedField ) -> List (Problem ValidatedField)
validateField (Trimmed form) ( _, field ) =
    List.map (InvalidEntry field) <|
        case field of
            Email ->
                if String.isEmpty form.email then
                    [ "email can't be blank." ]

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
trimFields : Form -> TrimmedForm Form
trimFields form =
    Trimmed
        { form
            | email = String.trim form.email
            , password = String.trim form.password
        }



-- VIEW


view : Model -> { title : String, body : Element Msg }
view model =
    { title = "Login"
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
            , width (px <| Brand.scaled 15)
            ]
            [ Layout.headerText "Login"
            , paragraph
                [ centerX
                , Font.color Brand.subtleTextColor
                , Font.size <| Brand.scaled 1
                , width shrink
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

            -- Errors
            , Layout.errNote (List.filter isServerError problems)

            -- Email
            , viewEmail form.email
                (List.filter (isFieldError Email) problems)

            -- Password
            , viewPassword form.password
                form.showPassword
                (List.filter (isFieldError Password) problems)

            -- Remember Me
            , viewRememberMe form.rememberMe

            -- Login
            , viewLoginBtn
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
            ok
        )
        problems


viewRememberMe : Bool -> Element Msg
viewRememberMe rememberMe =
    row
        [ spacing <| Brand.scaled -3 ]
        [ Input.checkbox [ width shrink ]
            { onChange = ToggleRememberMe
            , icon = Layout.radio True
            , checked = rememberMe
            , label =
                -- Input.labelHidden "Remember Me"
                -- },
                Input.labelRight
                    [ Font.color Brand.subtleTextColor
                    , Font.size <| Brand.scaled 1
                    , alignTop
                    , centerY
                    ]
                    (text "Remember Me")
            }
        ]


viewLoginBtn : Element Msg
viewLoginBtn =
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
        , label = el [ centerX, centerY ] (text "Login")
        }



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session

module Page.Login exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Api exposing (Cred)
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
    { email : String
    , password : String
    , showPassword : Bool
    , rememberMe : Bool
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
            updateForm (\form -> { form | email = email }) model

        ChangePassword password ->
            updateForm (\form -> { form | password = password }) model

        ToggleRememberMe rememberMe ->
            updateForm (\form -> { form | rememberMe = rememberMe }) model

        ToggleShowPassword show ->
            updateForm (\form -> { form | showPassword = show }) model

        SubmitForm ->
            case validate model.form of
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
                    , Cmd.batch
                        [ Viewer.store viewer
                        , Route.replaceUrl (Session.navKey model.session) Route.MyProfile
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
    = Email
    | Password


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Email
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
trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { form
            | email = String.trim form.email
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


view : Model -> { title : String, body : Element Msg }
view model =
    { title = "Login"
    , body =
        viewForm model.form model.problems
    }


viewForm : Form -> List Problem -> Element Msg
viewForm form problems =
    Layout.card <|
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
            , viewErrors (List.filter isServerError problems)

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
            ok
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
        (viewErrors problems)


viewRememberMe : Bool -> Element Msg
viewRememberMe rememberMe =
    row
        [ spacing <| Brand.scaled -3 ]
        [ Input.checkbox [ width shrink ]
            { onChange = ToggleRememberMe
            , icon = Layout.radio
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

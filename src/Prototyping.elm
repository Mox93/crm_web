module Prototyping exposing (..)

import API.Object
import API.Object.Login as LoginObject
import API.Object.Token as TokenObject
import API.Object.User as UserObject
import API.Query as Query
import API.Scalar exposing (Id(..))
import Browser
import Graphql.Http
import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import RemoteData exposing (RemoteData(..))



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    { email : String
    , password : String
    , response : RemoteData (Graphql.Http.Error Login) Login
    }


type alias Response =
    { msg : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model "" "" NotAsked, Cmd.none )



-- UPDATE


type Msg
    = Submit
    | ChangeEmail String
    | ChangePassword String
    | GotResponse (RemoteData (Graphql.Http.Error Login) Login)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit ->
            ( model, makeRequest model )

        ChangeEmail email ->
            ( { model | email = email }, Cmd.none )

        ChangePassword password ->
            ( { model | password = password }, Cmd.none )

        GotResponse response ->
            ( { model | response = response }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "margin" "10rem" ]
        [ label
            [ style "margin" "1rem" ]
            [ text "email" ]
        , input
            [ style "margin-right" "2rem"
            , type_ "text"
            , value model.email
            , onInput ChangeEmail
            ]
            []
        , label
            [ style "margin" "1rem"
            ]
            [ text "password" ]
        , input
            [ style "margin-right" "2rem"
            , type_ "password"
            , value model.password
            , onInput ChangePassword
            ]
            []
        , button
            [ onClick Submit ]
            [ text "submit" ]
        , div
            [ style "margin" "2rem" ]
            [ case model.response of
                NotAsked ->
                    text "Initialising."

                Loading ->
                    text "Loading."

                Failure err ->
                    case err of
                        Graphql.Http.GraphqlError _ gQLErr ->
                            ul [] <|
                                List.map (\str -> li [] [ text ("Error: " ++ str) ]) (List.map .message gQLErr)

                        Graphql.Http.HttpError httpErr ->
                            case httpErr of
                                Graphql.Http.BadUrl msg ->
                                    text msg

                                Graphql.Http.Timeout ->
                                    text "Timeout"

                                Graphql.Http.NetworkError ->
                                    text "Network Error"

                                Graphql.Http.BadStatus _ msg ->
                                    text msg

                                Graphql.Http.BadPayload _ ->
                                    text "Bad Payload"

                Success data ->
                    viewLogin data
            ]
        ]


viewLogin : Login -> Html Msg
viewLogin data =
    div []
        [ text "login: "
        , ul
            [ style "margin-right" "1rem" ]
            [ li []
                [ text "user:"
                , ul
                    [ style "margin-right" "2rem" ]
                    [ li [] [ text ("id: " ++ data.user.id) ]
                    , li [] [ text ("firstName: " ++ data.user.firstName) ]
                    , li [] [ text ("lastName: " ++ data.user.lastName) ]
                    , li [] [ text ("email: " ++ data.user.email) ]
                    , li [] [ text ("phoneNumber: " ++ data.user.phoneNumber) ]
                    , li []
                        [ text
                            ("company: "
                                ++ (case data.user.company of
                                        Just company ->
                                            company

                                        Nothing ->
                                            "N/A"
                                   )
                            )
                        ]
                    ]
                ]
            , li []
                [ text "token:"
                , ul
                    [ style "margin-right" "2rem" ]
                    [ li [] [ text ("access: " ++ data.token.access) ]
                    , li [] [ text ("refresh: " ++ data.token.refresh) ]
                    ]
                ]
            ]
        ]



-- API


api_url : String
api_url =
    "http://127.0.0.1:5000/api"


makeRequest : Model -> Cmd Msg
makeRequest model =
    let
        request =
            query model
    in
    request
        |> Graphql.Http.queryRequest api_url
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


query : Model -> SelectionSet Login RootQuery
query model =
    let
        form =
            { email = model.email, password = model.password }
    in
    Query.login
        (\optionals ->
            { optionals | rememberMe = Present True }
        )
        form
        loginDecoder


loginDecoder : SelectionSet Login API.Object.Login
loginDecoder =
    SelectionSet.map2 Login
        (LoginObject.user userDecoder)
        (LoginObject.token tokenDecoder)


userDecoder : SelectionSet User API.Object.User
userDecoder =
    SelectionSet.map6 User
        (SelectionSet.map (\(Id id) -> id) UserObject.id)
        UserObject.firstName
        UserObject.lastName
        UserObject.email
        UserObject.phoneNumber
        (SelectionSet.map companyDecoder UserObject.company)


companyDecoder : Maybe Id -> Maybe String
companyDecoder maybeId =
    case maybeId of
        Just (Id id) ->
            Just id

        Nothing ->
            Nothing


tokenDecoder : SelectionSet Token API.Object.Token
tokenDecoder =
    SelectionSet.map2 Token
        TokenObject.access
        TokenObject.refresh


type alias Login =
    { user : User
    , token : Token
    }


type alias User =
    { id : String
    , firstName : String
    , lastName : String
    , email : String
    , phoneNumber : String
    , company : Maybe String
    }


type alias Token =
    { access : String
    , refresh : String
    }

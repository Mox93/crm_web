port module Api exposing
    ( Cred
    , application
    , login
    , logout
    , makePrivetMutation
    , makePrivetQuery
    , makePublicMutation
    , makePublicQuery
    , signup
    , storeCredWith
    , viewerChanges
    )

{-| This module is responsible for communicating to the API.
-}

import API.Mutation as Mutation
import API.Object
import API.Object.Login as LoginObject
import API.Object.Signup as SignupObject
import API.Object.Token as TokenObject
import API.Query as Query
import Browser
import Browser.Navigation as Nav
import Graphql.Http
import Graphql.Operation exposing (RootMutation, RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder, Value)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import RemoteData exposing (RemoteData)
import Url exposing (Url)
import User exposing (User)



-- CREDENTIALS


{-| The authentication credentials for the Viewer (that is, the currently logged-in user.)
This includes:

  - The cred's User
  - The cred's Token
    By design, there is no way to access the token directly as a String.
    It can be encoded for persistence, and it can be added to a header
    to a HttpBuilder for a request, but that's it.
    This token should never be rendered to the end user, and with this API, it
    can't be!

-}
type Cred
    = Cred AccessToken RefreshToken


type AccessToken
    = AccessToken String


type RefreshToken
    = RefreshToken String


{-| It's important that this is never exposed!
We expose `login` and `application` instead, so we can be certain that if anyone
ever has access to a `Cred` value, it came from either the login API endpoint
or was passed in via flags.
-}
credSelectionSet : SelectionSet Cred API.Object.Token
credSelectionSet =
    SelectionSet.map2 Cred
        (SelectionSet.map (\str -> AccessToken str) TokenObject.access)
        (SelectionSet.map (\str -> RefreshToken str) TokenObject.refresh)


credDecoder : Decoder Cred
credDecoder =
    Decode.succeed Cred
        |> required "access" (Decode.map (\str -> AccessToken str) Decode.string)
        |> required "refresh" (Decode.map (\str -> RefreshToken str) Decode.string)



-- GraphQL


api_url : String
api_url =
    "http://127.0.0.1:5000/api"


getAuthHeader : String -> (Graphql.Http.Request decodesTo -> Graphql.Http.Request decodesTo)
getAuthHeader token =
    Graphql.Http.withHeader "Authorization" ("Bearer " ++ token)


makePublicQuery :
    SelectionSet decodesTo RootQuery
    -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg)
    -> Cmd msg
makePublicQuery query decodesTo =
    query
        |> Graphql.Http.queryRequest api_url
        |> Graphql.Http.send decodesTo


makePrivetQuery :
    Cred
    -> SelectionSet decodesTo RootQuery
    -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg)
    -> Cmd msg
makePrivetQuery (Cred (AccessToken token) _) query decodesTo =
    query
        |> Graphql.Http.queryRequest api_url
        {-
           queryRequest signature is of the form
               String -> SelectionSet decodesTo RootQuery -> Request decodesTo
               url    -> SelectionSet TasksWUser RootQuery -> Request TasksWUser
        -}
        |> getAuthHeader token
        |> Graphql.Http.send decodesTo


makePublicMutation :
    SelectionSet decodesTo RootMutation
    -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg)
    -> Cmd msg
makePublicMutation mutation decodesTo =
    mutation
        |> Graphql.Http.mutationRequest api_url
        |> Graphql.Http.send decodesTo


makePrivetMutation :
    Cred
    -> SelectionSet decodesTo RootMutation
    -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg)
    -> Cmd msg
makePrivetMutation (Cred (AccessToken token) _) mutation decodesTo =
    mutation
        |> Graphql.Http.mutationRequest api_url
        |> getAuthHeader token
        |> Graphql.Http.send decodesTo


refreshToken :
    Cred
    -> SelectionSet decodesTo RootQuery
    -> (Result (Graphql.Http.Error decodesTo) decodesTo -> msg)
    -> Cmd msg
refreshToken (Cred _ (RefreshToken token)) query decodesTo =
    query
        |> Graphql.Http.queryRequest api_url
        |> getAuthHeader token
        |> Graphql.Http.send decodesTo


login :
    { form
        | email : String
        , password : String
        , rememberMe : Bool
    }
    -> (RemoteData (Graphql.Http.Error viewer) viewer -> msg)
    ->
        ((SelectionSet User API.Object.User -> SelectionSet User API.Object.Login)
         -> (SelectionSet Cred API.Object.Login -> SelectionSet viewer API.Object.Login)
        )
    -> Cmd msg
login form msg viewerSelectionSet =
    let
        requiredFields =
            { email = form.email
            , password = form.password
            }

        optionalFields optionals =
            { optionals | rememberMe = Present form.rememberMe }
    in
    makePublicQuery
        (Query.login optionalFields
            requiredFields
            (viewerSelectionSet LoginObject.user <|
                LoginObject.token credSelectionSet
            )
        )
        (RemoteData.fromResult >> msg)


signup :
    { form
        | firstName : String
        , lastName : String
        , email : String
        , phoneNumber : String
        , password : String
    }
    -> (RemoteData (Graphql.Http.Error viewer) viewer -> msg)
    ->
        ((SelectionSet User API.Object.User -> SelectionSet User API.Object.Signup)
         -> (SelectionSet Cred API.Object.Signup -> SelectionSet viewer API.Object.Signup)
        )
    -> Cmd msg
signup form msg viewerSelectionSet =
    let
        requiredFields =
            { userData =
                { firstName = form.firstName
                , lastName = form.lastName
                , email = form.email
                , phoneNumber = form.phoneNumber
                , password = form.password
                }
            }
    in
    makePublicMutation
        (Mutation.signup
            requiredFields
            (viewerSelectionSet SignupObject.user <|
                SignupObject.token credSelectionSet
            )
        )
        (RemoteData.fromResult >> msg)



-- PERSISTENCE
{-
   decode : Decoder (Cred -> viewer) -> Value -> Result Decode.Error viewer
   decode decoder value =
       -- It's stored in localStorage as a JSON String;
       -- first decode the Value as a String, then
       -- decode that String as JSON.
       Decode.decodeValue Decode.string value
           |> Result.andThen (\str -> Decode.decodeString (Decode.field "user" (decoderFromCred decoder)) str)
-}


port onStoreChange : (Value -> msg) -> Sub msg


viewerChanges : (Maybe viewer -> msg) -> Decoder (Cred -> viewer) -> Sub msg
viewerChanges toMsg decoder =
    onStoreChange (\value -> toMsg (decodeFromChange decoder value))


decodeFromChange : Decoder (Cred -> viewer) -> Value -> Maybe viewer
decodeFromChange viewerDecoder val =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Decode.decodeValue (storageDecoder viewerDecoder) val
        |> Result.toMaybe


storeCredWith : Cred -> User -> Cmd msg
storeCredWith (Cred (AccessToken access) (RefreshToken refresh)) user =
    let
        json =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "info", User.encode user )
                        , ( "access", Encode.string access )
                        , ( "refresh", Encode.string refresh )
                        ]
                  )
                ]
    in
    storeCache (Just json)


logout : Cmd msg
logout =
    storeCache Nothing


port storeCache : Maybe Value -> Cmd msg



-- APPLICATION


application :
    Decoder (Cred -> viewer)
    ->
        { init : Maybe viewer -> Url -> Nav.Key -> ( model, Cmd msg )
        , onUrlChange : Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , subscriptions : model -> Sub msg
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        }
    -> Program Value model msg
application viewerDecoder config =
    let
        init flags url navKey =
            let
                maybeViewer =
                    Decode.decodeValue Decode.string flags
                        |> Result.andThen (Decode.decodeString (storageDecoder viewerDecoder))
                        |> Result.toMaybe
            in
            config.init maybeViewer url navKey
    in
    Browser.application
        { init = init
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }


storageDecoder : Decoder (Cred -> viewer) -> Decoder viewer
storageDecoder viewerDecoder =
    Decode.field "user" (decoderFromCred viewerDecoder)


decoderFromCred : Decoder (Cred -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map2 (\fromCred cred -> fromCred cred)
        decoder
        credDecoder

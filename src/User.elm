module User exposing (User, decoder, encode, fullName, selectionSet)

import API.Object
import API.Object.User as UserObject
import API.Scalar exposing (Id(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)



-- USER


type alias User =
    { id : String
    , firstName : String
    , lastName : String
    , email : String
    , phoneNumber : String
    , company : Maybe String
    }


fullName : User -> String
fullName user =
    user.firstName ++ " " ++ user.lastName



-- DECODERS


decoder : Decoder User
decoder =
    Decode.succeed User
        |> required "id" Decode.string
        |> required "firstName" Decode.string
        |> required "lastName" Decode.string
        |> required "email" Decode.string
        |> required "phoneNumber" Decode.string
        |> required "company" (Decode.nullable Decode.string)


encode : User -> Value
encode user =
    Encode.object
        [ ( "id", Encode.string user.id )
        , ( "firstName", Encode.string user.firstName )
        , ( "lastName", Encode.string user.lastName )
        , ( "email", Encode.string user.email )
        , ( "phoneNumber", Encode.string user.phoneNumber )
        , ( "company"
          , case user.company of
                Just val ->
                    Encode.string val

                Nothing ->
                    Encode.null
          )
        ]


selectionSet : SelectionSet User API.Object.User
selectionSet =
    SelectionSet.map6 User
        (SelectionSet.map (\(Id id) -> id) UserObject.id)
        UserObject.firstName
        UserObject.lastName
        UserObject.email
        UserObject.phoneNumber
        (SelectionSet.map maybeIdToMaybeString UserObject.company)


maybeIdToMaybeString : Maybe Id -> Maybe String
maybeIdToMaybeString maybeId =
    case maybeId of
        Just (Id id) ->
            Just id

        Nothing ->
            Nothing

module ErrorHandling exposing
    ( Problem(..)
    , TrimmedForm(..)
    , graphqlErrorToProblem
    , httpErrorToString
    , isFieldError
    , isServerError
    , problemToString
    , removeFieldError
    , validate
    )

import Graphql.Http
import Graphql.Http.GraphqlError exposing (GraphqlError)
import Json.Decode as Decode



-- TYPES


type Problem validatedField
    = InvalidEntry validatedField String
    | ServerError String


{-| Marks that we've trimmed the form's fields, so we don't accidentally send
it to the server without having trimmed it!
-}
type TrimmedForm form
    = Trimmed form



-- EXPORT


graphqlErrorToProblem : List ( String, validatedField ) -> GraphqlError -> List (Problem validatedField)
graphqlErrorToProblem errMap err =
    let
        errList =
            Decode.decodeString (Decode.keyValuePairs Decode.string) err.message
    in
    case errList of
        Err _ ->
            [ ServerError err.message ]

        Ok errors ->
            List.map (convertToProblem errMap) errors


httpErrorToString : Graphql.Http.HttpError -> String
httpErrorToString httpErr =
    case httpErr of
        Graphql.Http.BadUrl errMsg ->
            errMsg

        Graphql.Http.Timeout ->
            "Timeout"

        Graphql.Http.NetworkError ->
            "Network Error"

        Graphql.Http.BadStatus _ errMsg ->
            errMsg

        Graphql.Http.BadPayload _ ->
            "Bad Payload"


isServerError : Problem validatedField -> Bool
isServerError problem =
    case problem of
        ServerError _ ->
            True

        _ ->
            False


isFieldError : validatedField -> Problem validatedField -> Bool
isFieldError field problem =
    case problem of
        InvalidEntry f _ ->
            if f == field then
                True

            else
                False

        ServerError _ ->
            False


removeFieldError : validatedField -> List (Problem validatedField) -> List (Problem validatedField)
removeFieldError field problems =
    List.filter (\err -> not (isFieldError field err)) problems


problemToString : Problem validatedField -> String
problemToString problem =
    case problem of
        InvalidEntry _ str ->
            str

        ServerError str ->
            str


{-| Trim the form and validate its fields. If there are problems, report them!
-}
validate :
    TrimmedForm form
    -> List ( String, validatedField )
    -> (TrimmedForm form -> ( String, validatedField ) -> List (Problem validatedField))
    -> Result (List (Problem validatedField)) (TrimmedForm form)
validate trimmedForm fieldsToValidate validateField =
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems



-- INTERNAL


convertToProblem : List ( String, validatedField ) -> ( String, String ) -> Problem validatedField
convertToProblem errMap ( key, val ) =
    case List.filter (\( name, _ ) -> name == key) errMap of
        [ ( _, field ) ] ->
            InvalidEntry field val

        _ ->
            ServerError val

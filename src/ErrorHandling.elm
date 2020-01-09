module ErrorHandling exposing (Problem(..), httpErrorToString)

import Graphql.Http



-- TYPES


type Problem v
    = InvalidEntry v String
    | ServerError String


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

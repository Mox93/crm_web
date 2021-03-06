-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module API.Scalar exposing (Codecs, DateTime(..), Id(..), defaultCodecs, defineCodecs, unwrapCodecs, unwrapEncoder)

import Graphql.Codec exposing (Codec)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type DateTime
    = DateTime String


type Id
    = Id String


defineCodecs :
    { codecDateTime : Codec valueDateTime
    , codecId : Codec valueId
    }
    -> Codecs valueDateTime valueId
defineCodecs definitions =
    Codecs definitions


unwrapCodecs :
    Codecs valueDateTime valueId
    ->
        { codecDateTime : Codec valueDateTime
        , codecId : Codec valueId
        }
unwrapCodecs (Codecs unwrappedCodecs) =
    unwrappedCodecs


unwrapEncoder getter (Codecs unwrappedCodecs) =
    (unwrappedCodecs |> getter |> .encoder) >> Graphql.Internal.Encode.fromJson


type Codecs valueDateTime valueId
    = Codecs (RawCodecs valueDateTime valueId)


type alias RawCodecs valueDateTime valueId =
    { codecDateTime : Codec valueDateTime
    , codecId : Codec valueId
    }


defaultCodecs : RawCodecs DateTime Id
defaultCodecs =
    { codecDateTime =
        { encoder = \(DateTime raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map DateTime
        }
    , codecId =
        { encoder = \(Id raw) -> Encode.string raw
        , decoder = Object.scalarDecoder |> Decode.map Id
        }
    }

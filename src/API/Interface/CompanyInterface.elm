-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module API.Interface.CompanyInterface exposing (..)

import API.InputObject
import API.Interface
import API.Object
import API.Scalar
import API.ScalarCodecs
import API.Union
import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (FragmentSelectionSet(..), SelectionSet(..))
import Json.Decode as Decode


type alias Fragments decodesTo =
    { onCompany : SelectionSet decodesTo API.Object.Company
    }


{-| Build an exhaustive selection of type-specific fragments.
-}
fragments :
    Fragments decodesTo
    -> SelectionSet decodesTo API.Interface.CompanyInterface
fragments selections =
    Object.exhuastiveFragmentSelection
        [ Object.buildFragment "Company" selections.onCompany
        ]


{-| Can be used to create a non-exhuastive set of fragments by using the record
update syntax to add `SelectionSet`s for the types you want to handle.
-}
maybeFragments : Fragments (Maybe decodesTo)
maybeFragments =
    { onCompany = Graphql.SelectionSet.empty |> Graphql.SelectionSet.map (\_ -> Nothing)
    }


id : SelectionSet API.ScalarCodecs.Id API.Interface.CompanyInterface
id =
    Object.selectionForField "ScalarCodecs.Id" "Id" [] (API.ScalarCodecs.codecs |> API.Scalar.unwrapCodecs |> .codecId |> .decoder)


creationDate : SelectionSet API.ScalarCodecs.DateTime API.Interface.CompanyInterface
creationDate =
    Object.selectionForField "ScalarCodecs.DateTime" "creationDate" [] (API.ScalarCodecs.codecs |> API.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


modifiedDate : SelectionSet API.ScalarCodecs.DateTime API.Interface.CompanyInterface
modifiedDate =
    Object.selectionForField "ScalarCodecs.DateTime" "modifiedDate" [] (API.ScalarCodecs.codecs |> API.Scalar.unwrapCodecs |> .codecDateTime |> .decoder)


name : SelectionSet (Maybe String) API.Interface.CompanyInterface
name =
    Object.selectionForField "(Maybe String)" "name" [] (Decode.string |> Decode.nullable)


email : SelectionSet (Maybe String) API.Interface.CompanyInterface
email =
    Object.selectionForField "(Maybe String)" "email" [] (Decode.string |> Decode.nullable)


phoneNumber : SelectionSet (Maybe String) API.Interface.CompanyInterface
phoneNumber =
    Object.selectionForField "(Maybe String)" "phoneNumber" [] (Decode.string |> Decode.nullable)


productCategories : SelectionSet decodesTo API.Object.ProductCategory -> SelectionSet (Maybe (List (Maybe decodesTo))) API.Interface.CompanyInterface
productCategories object_ =
    Object.selectionForCompositeField "productCategories" [] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)

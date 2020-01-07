module Data.Category exposing (..)

import Data.Link as Link exposing (Link)
import Dict
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as Encode
import RemoteData exposing (WebData)


type alias CategoryId =
    Int


type alias ModCategory =
    { formIsHidden : Bool
    , id : CategoryId
    , isEditingCategoryTitle : Bool
    , mods : List Link
    , newDescription : String
    , newTitle : String -- new link title -- todo move to form
    , newUrl : String
    , order : Int
    , savingState : WebData ()
    , suggestedTitle : WebData String
    , newName : String -- new category title
    , name : String
    }



-- decode


decodeModCategory =
    Decode.succeed ModCategory
        |> Decode.hardcoded True
        |> Decode.required "id" Decode.int
        |> Decode.hardcoded False
        |> Decode.optional "links" (Decode.list Link.decode) []
        |> Decode.hardcoded ""
        |> Decode.hardcoded ""
        |> Decode.hardcoded ""
        |> Decode.required "order" Decode.int
        |> Decode.hardcoded RemoteData.NotAsked
        |> Decode.hardcoded RemoteData.NotAsked
        |> Decode.required "name" Decode.string
        |> Decode.required "name" Decode.string


decodeModCategories =
    Decode.field "categories" (Decode.list decodeModCategory)
        |> Decode.andThen
            (\categories ->
                categories
                    |> List.map (\c -> ( c.id, c ))
                    |> Dict.fromList
                    |> Decode.succeed
            )



-- encode


encode { name, order, owner } =
    -- @TODO owner_id
    Encode.object
        [ ( "name", Encode.string name )
        , ( "order", Encode.int order )
        , ( "owner", Encode.int 1 )
        ]



-- query


queryDocument =
    """
query Categories {
    categories{
        name, order, id, links {
            id, title, urlString
        }
    }
}
"""


insert =
    """
mutation InsertCategory($objects: [categories_insert_input!]!) {
  __typename
  insert_categories(objects: $objects) {
    returning {
      id, name, order
    }
  }
}

"""

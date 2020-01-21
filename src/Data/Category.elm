module Data.Category exposing (..)

import Data.Link as Link exposing (Link)
import Dict
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as Encode
import RemoteData exposing (WebData)


type alias CategoryId =
    String


type alias Category =
    { formIsHidden : Bool
    , id : CategoryId
    , isEditingCategoryTitle : Bool
    , links : List Link
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


decodeCategory =
    Decode.succeed Category
        |> Decode.hardcoded True
        |> Decode.required "id" Decode.string
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
    Decode.field "categories" (Decode.list decodeCategory)
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
        , ( "owner", Encode.string owner )
        ]



-- query


queryDocument =
    """
query Categories {
    categories(order_by: {order: desc}){
        name, order, id, links(where: {soft_delete: {_eq: false}}) {
            id, title, urlString, active, soft_delete, description
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


update =
    """
mutation UpdateCategory($id:uuid!, $name:String!) {
  __typename
  update_categories(where: {id: {_eq: $id}}, _set: {name: $name}) {
    returning {
      id, name, order, links {id, urlString, title, description, active}
    }
  }
}

"""

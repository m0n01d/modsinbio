module Data.Link exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as Encode
import Url


type Id
    = Id Int


id (Id identifier) =
    identifier


encodeId (Id identifier) =
    Encode.int identifier


type MorePanel
    = DeletionPanel
    | AnalyticsPanel


type alias Link =
    { id : Id
    , url : Maybe Url.Url
    , urlString : String
    , title : String
    , description : String
    , panel : Maybe MorePanel
    , isActive : Bool
    }



-- decode


decodeToMaybeUrl =
    Decode.string
        |> Decode.map Url.fromString


decodeId =
    Decode.int
        |> Decode.map Id


decode =
    Decode.succeed Link
        |> Decode.required "id" decodeId
        |> Decode.custom (Decode.field "urlString" decodeToMaybeUrl)
        |> Decode.required "urlString" Decode.string
        |> Decode.required "title" Decode.string
        |> Decode.optional "description" Decode.string ""
        |> Decode.hardcoded Nothing
        |> Decode.required "active" Decode.bool



-- encode


protocolToString p =
    case p of
        Url.Https ->
            "https"

        Url.Http ->
            "http"


encode { urlString, description, title, category_id, fragment, host, path, protocol, query } =
    Encode.object
        [ ( "urlString", Encode.string urlString )
        , ( "description", Encode.string description )
        , ( "title", Encode.string title )
        , ( "category_id", Encode.int category_id )
        , ( "fragment", Encode.maybe Encode.string fragment )
        , ( "host", Encode.string host )
        , ( "path", Encode.string path )
        , ( "protocol", Encode.string <| protocolToString protocol )
        , ( "query", Encode.maybe Encode.string query )
        ]



-- query


insert =
    """
mutation InsertLink($objects: [links_insert_input!]!) {
  __typename
  insert_links(objects: $objects) {
    returning {
      id, title, urlString, description, active
    }
  }
}

"""


updateIsActive =
    """
mutation UpdateIsActive($id:Int!, $is_active:Boolean!) {
  __typename
  update_links(_set:{active : $is_active} where: {id: {_eq: $id}}) {
    returning {
\t\t\tid, title, urlString, description, active
    }
  }
}

"""


deleteLink =
    """
mutation DeleteLInk($id: Int) {
  __typename
  delete_links(where: {id: {_eq: $id}}) {
    returning {
      id
    }
  }
}

"""

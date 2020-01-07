module Data.Link exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as Encode
import Url


type MorePanel
    = DeletionPanel
    | AnalyticsPanel


type alias Link =
    { id : Int
    , url : Maybe Url.Url
    , urlString : String
    , title : String
    , description : String
    , panel : Maybe MorePanel
    }



-- decode


decodeToMaybeUrl =
    Decode.string
        |> Decode.map Url.fromString


decode =
    Decode.succeed Link
        |> Decode.required "id" Decode.int
        |> Decode.custom (Decode.field "urlString" decodeToMaybeUrl)
        |> Decode.required "urlString" Decode.string
        |> Decode.required "title" Decode.string
        |> Decode.optional "description" Decode.string ""
        |> Decode.hardcoded Nothing



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
      id, title, urlString, description
    }
  }
}

"""
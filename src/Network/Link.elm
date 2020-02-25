module Network.Link exposing (..)

import Data.Link as Link exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Network.Api as Api


addLink =
    """
mutation LinkClicked($id:uuid!) {
  __typename
  insert_link_clicks(objects: {link: $id}) {
    affected_rows
  }
}

"""


clickedMutation id =
    let
        vars =
            Encode.object
                [ ( "id", Link.encodeId id ) ]
                |> Just
    in
    Api.unauthedQuery (Api.document addLink []) vars (Decode.succeed ())

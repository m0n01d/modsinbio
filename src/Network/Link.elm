module Network.Link exposing (clickedMutation)

import Data.Link as Link
import Json.Decode as Decode
import Json.Encode as Encode
import Network.Api as Api
import Task exposing (Task)


addLink : String
addLink =
    """
mutation LinkClicked($id:uuid!) {
  __typename
  insert_link_clicks(objects: {link: $id}) {
    affected_rows
  }
}

"""


clickedMutation : Link.Id -> Task Api.Error ()
clickedMutation id =
    let
        vars =
            Encode.object
                [ ( "id", Link.encodeId id ) ]
                |> Just
    in
    Api.unauthedQuery (Api.document addLink []) vars (Decode.succeed ())

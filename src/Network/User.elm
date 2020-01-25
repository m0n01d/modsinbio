module Network.User exposing (..)

import Data.User as User exposing (User(..))
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Network.Api as Api
import Url.Builder



-- graphql


fetchUser =
    """
query FetchUser {
  __typename
  users {
    username
    id
    profile
    views
    categories {
      id
      name
      links {
        id
      }
    }
  }
}

"""


fetchProfile =
    """
query FetchProfile($profile:String!) {
  users(where: {username: {_eq: $profile}})
   {
    id, username, profile,
    categories(order_by: {order: desc}) {
      id, name, order, links(where: {soft_delete: {_eq: false}}) {
        id, active, title, urlString, description
      }
    }
  }
}

"""


updateProfile =
    """
mutation UpdateProfile($id:uuid!,$profile: jsonb) {
  __typename
  update_users(_set: {profile: $profile}, where: {id: {_eq: $id}}) {
    returning {
      id
    }
  }
}


  """


document =
    Api.document fetchUser []


decoder_ =
    Decode.succeed identity
        |> Decode.required "users" (Decode.index 0 User.decodeDriverProfile)


query session token =
    Api.authedQuery token document Nothing decoder_


sessionToken { user } =
    case user of
        Public ->
            ""

        DriverPartial token ->
            token

        Driver token _ ->
            token


profileDecoder_ =
    Decode.succeed identity
        |> Decode.required "users" (Decode.index 0 User.decodePublicProfile)


profileQuery username =
    let
        vars =
            Encode.object [ ( "profile", Encode.string username ) ]
                |> Just
    in
    Api.unauthedQuery (Api.document fetchProfile []) vars profileDecoder_


updateUserMutation token id profile =
    let
        -- {"id":"fd1f55d0-b14f-4ccc-8821-4cbf5f0710a6", "profile": {"bio":"it dwit"}}
        vars =
            Encode.object
                [ ( "id"
                  , Encode.string (User.idToString id)
                  )
                , ( "profile", User.encodeProfile profile )
                ]
                |> Just
    in
    Api.authedQuery token (Api.document updateProfile []) vars (Decode.succeed ())


incrementViewCount { id } onComplete =
    let
        vars =
            [ Url.Builder.string "id" (User.idToString id)
            ]
    in
    Http.get
        { url = Url.Builder.absolute [ "api", "analytics-viewed" ] vars
        , expect = Http.expectWhatever onComplete
        }

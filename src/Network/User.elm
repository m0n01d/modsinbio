module Network.User exposing (..)

import Data.User as User exposing (User(..))
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Network.Api as Api



-- graphql


fetchUser =
    """
query FetchUser {
  __typename
  users {
    username
    id
    bio
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
    id, username, bio,
    categories(order_by: {order: desc}) {
      id, name, order, links(where: {soft_delete: {_eq: false}}) {
        id, active, title, urlString, description
      }
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


profileDocument username =
    let
        vars =
            Encode.object [ ( "profile", Encode.string username ) ]
                |> Just
    in
    Api.unauthedQuery (Api.document fetchProfile []) vars profileDecoder_

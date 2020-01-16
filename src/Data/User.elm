module Data.User exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Network.Api as Api


type User
    = Public
    | DriverPartial AccessToken
    | Driver AccessToken DriverProfile


driverPartialToFull : User -> DriverProfile -> User
driverPartialToFull user profile =
    case user of
        DriverPartial token ->
            Driver token profile

        _ ->
            user


decodeDriverPartial =
    Decode.succeed DriverPartial
        |> Decode.required "token" Decode.string


decodeDriver =
    Decode.succeed Driver
        |> Decode.requiredAt [ "payload", "token" ] Decode.string
        |> Decode.requiredAt [ "payload", "user" ] decodeDriverProfile


decodeDriverProfile =
    Decode.succeed DriverProfile
        |> Decode.required "id" (Decode.map UserId Decode.string)
        |> Decode.required "username" Decode.string
        |> Decode.optional "bio" Decode.string ""


decoder =
    Decode.oneOf [ decodeDriver, decodePublic ]


decodePublic =
    Decode.succeed Public


type UserId
    = UserId String


idToString (UserId identifier) =
    identifier


type alias DriverProfile =
    { id : UserId
    , username : String
    , bio : String
    }


type alias AccessToken =
    String



-- encode


encodeDriverProfile : DriverProfile -> Value
encodeDriverProfile { id, username, bio } =
    Encode.object
        [ ( "id", Encode.string <| idToString id )
        , ( "username", Encode.string username )
        , ( "bio", Encode.string bio )
        ]


encodeDriver : String -> DriverProfile -> Value
encodeDriver token driverProfile =
    Encode.object
        [ ( "token", Encode.string token )
        , ( "user", encodeDriverProfile driverProfile )
        ]



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


document =
    Api.document fetchUser []


decoder_ =
    Decode.succeed identity
        |> Decode.required "users" (Decode.index 0 decodeDriverProfile)


query session token =
    Api.query session token document Nothing decoder_


sessionToken { user } =
    case user of
        Public ->
            ""

        DriverPartial token ->
            token

        Driver token _ ->
            token

module Data.User exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
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
        |> Decode.required "payload" decodeDriverProfile


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


type alias DriverProfile =
    { id : UserId
    , username : String
    , bio : String
    }


type alias AccessToken =
    String


foo =
    Decode.decodeString decodeDriver xxx
        |> Debug.log "foo"


xxx =
    "{\"id\":\"fd1f55d0-b14f-4ccc-8821-4cbf5f0710a6\",\"username\":\"dwrxht\",\"roles\":[\"driver\",\"public\"],\"token\":\"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9.eyJuYW1lIjoiZHdyeGh0IiwiaHR0cHM6Ly9oYXN1cmEuaW8vand0L2NsYWltcyI6eyJ4LWhhc3VyYS1hbGxvd2VkLXJvbGVzIjpbImRyaXZlciIsInB1YmxpYyJdLCJ4LWhhc3VyYS1kZWZhdWx0LXJvbGUiOiJkcml2ZXIiLCJ4LWhhc3VyYS11c2VyLWlkIjoiZmQxZjU1ZDAtYjE0Zi00Y2NjLTg4MjEtNGNiZjVmMDcxMGE2In0sImlhdCI6MTU3ODk4MDA3NSwiZXhwIjoxNTgxNTcyMDc1LCJzdWIiOiJmZDFmNTVkMC1iMTRmLTRjY2MtODgyMS00Y2JmNWYwNzEwYTYifQ.kx8z4viCchRbSmweCLChEsw16WroITLELJpmgBPoKC2iKtvnSfUAwD6Wkp0B9arkEaHFn4htxErd8ZAtbLba9zAydCOQbq3bZjjtoAhjf_rUYFaxXXy9cYiK8aoe4xi6VdzIPZk64SYBJkNIeSY-wMd4rILqZImasqQQi6qxNvHbpUBrob6w5f_Uf6xZn-SYOS35VXyxZEJ_qrdIbYWceNM4Ak6ugYPj7KQstb5HYgmL1J2nrmnCEF9Ya3Ol6-ugRx1AtDaBiiW63jmws_7D3ViDOzaWxdWPVDU1vcGq94yrQd6rAftg11e97Hc-YxWeNt80-1CzxX_YWbZF-do-1w\"}"


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

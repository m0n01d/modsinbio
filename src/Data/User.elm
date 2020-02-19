module Data.User exposing (..)

import Data.Category as Category exposing (Category, CategoryId)
import Dict exposing (Dict)
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as Encode


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
        |> Decode.optional "username" Decode.string ""
        |> Decode.custom (Decode.field "profile" (Decode.nullable decodeProfileData))
        |> Decode.optional "views" Decode.int 0


decodeProfileData =
    Decode.succeed Profile
        |> Decode.optional "vehicleYear" Decode.string ""
        |> Decode.optional "vehicleMake" Decode.string ""
        |> Decode.optional "vehicleModel" Decode.string ""
        |> Decode.optional "bio" Decode.string ""


decoder =
    Decode.oneOf [ decodeDriver, decodePublic ]


decodePublic =
    Decode.succeed Public


type UserId
    = UserId String


idToString (UserId identifier) =
    identifier


stringToId userId =
    UserId userId


type alias DriverProfile =
    { id : UserId
    , username : String
    , profile : Maybe Profile
    , views : Int
    }


type alias Profile =
    { vehicleYear : String
    , vehicleMake : String
    , vehicleModel : String
    , bio : String
    }


type alias AccessToken =
    String


type alias Mods =
    Dict CategoryId Category


type alias PublicProfile =
    { profile : DriverProfile
    , mods : Mods
    }


decodePublicProfile =
    Decode.succeed PublicProfile
        |> Decode.custom decodeDriverProfile
        |> Decode.custom Category.decodeModCategories



-- TODO ME DADDY
-- encode


encodeDriverProfile : DriverProfile -> Value
encodeDriverProfile { id, username, profile } =
    Encode.object
        [ ( "id", Encode.string <| idToString id )
        , ( "username", Encode.string username )
        , ( "profile", Encode.maybe encodeProfile profile )
        ]


encodeProfile { bio, vehicleMake, vehicleModel, vehicleYear } =
    Encode.object
        [ ( "bio", Encode.string bio )
        , ( "vehicleMake", Encode.string vehicleMake )
        , ( "vehicleModel", Encode.string vehicleModel )
        , ( "vehicleYear", Encode.string vehicleYear )
        ]


encodeDriver : String -> DriverProfile -> Value
encodeDriver token driverProfile =
    Encode.object
        [ ( "token", Encode.string token )
        , ( "user", encodeDriverProfile driverProfile )
        ]

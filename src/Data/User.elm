module Data.User exposing (..)

import Data.Category as Category exposing (Category, CategoryId)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Json.Encode.Extra as Encode


type User
    = Partial AccessToken
    | Driver Driver


type Driver
    = DriverFull AccessToken DriverProfile


driverPartialToFull : DriverProfile -> User -> User
driverPartialToFull driverProfile user =
    case user of
        Partial token ->
            Driver <| DriverFull token driverProfile

        _ ->
            user


decodeDriverPartial : Decoder User
decodeDriverPartial =
    Decode.succeed Partial
        |> Decode.required "token" Decode.string


decoder : Decoder User
decoder =
    Decode.succeed identity
        |> Decode.custom decodeDriver_
        |> Decode.map Driver


decodeDriver_ : Decoder Driver
decodeDriver_ =
    Decode.succeed DriverFull
        |> Decode.requiredAt [ "token" ] Decode.string
        |> Decode.requiredAt [ "user" ] decodeDriverProfile


decodeDriverProfile =
    Decode.succeed DriverProfile
        |> Decode.required "id" (Decode.map UserId Decode.string)
        |> Decode.custom (Decode.field "username" (Decode.nullable Decode.string))
        |> Decode.custom (Decode.field "profile" (Decode.nullable decodeProfileData))
        |> Decode.optional "views" Decode.int 0


decodeProfileData =
    Decode.succeed Profile
        |> Decode.optional "vehicleYear" Decode.string ""
        |> Decode.optional "vehicleMake" Decode.string ""
        |> Decode.optional "vehicleModel" Decode.string ""
        |> Decode.optional "bio" Decode.string ""


type UserId
    = UserId String


idToString (UserId identifier) =
    identifier


stringToId userId =
    UserId userId


type alias DriverProfile =
    { id : UserId
    , username : Maybe String
    , profile : Maybe Profile
    , views : Int
    }


type alias Profile =
    { bio : String
    , vehicleMake : String
    , vehicleModel : String
    , vehicleYear : String
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
        , ( "username", Encode.maybe Encode.string username )
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

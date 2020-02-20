port module Data.Session exposing (Session, navKey, saveUser)

import Browser.Navigation as Nav
import Data.User as User exposing (User)
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)


type alias Session =
    { key : Nav.Key
    , user : User
    }


navKey : Session -> Nav.Key
navKey { key } =
    key


port toJs : Value -> Cmd msg


saveUser token driverProfile =
    let
        encodedUser =
            User.encodeDriver token driverProfile

        payload =
            Encode.object [ ( "payload", encodedUser ), ( "tag", Encode.string "SAVE_USER" ) ]
    in
    toJs payload

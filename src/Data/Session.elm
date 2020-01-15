module Data.Session exposing (Session, navKey)

import Browser.Navigation as Nav
import Data.User as User exposing (User)


type alias Session =
    { key : Nav.Key
    , user : User
    }


navKey : Session -> Nav.Key
navKey { key } =
    key

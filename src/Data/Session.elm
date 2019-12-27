module Data.Session exposing (Session, navKey)

import Browser.Navigation as Nav


type alias Session =
    { key : Nav.Key }


navKey : Session -> Nav.Key
navKey { key } =
    key

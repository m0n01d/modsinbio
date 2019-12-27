module Page.MyMods exposing (..)

import Data.Session exposing (Session)
import Html exposing (Html)


type alias Model =
    { session : Session }


view =
    Html.text "My mods"

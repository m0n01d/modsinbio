module Page.Settings exposing (Model, view)

import Data.Session exposing (Session)
import Html


type alias Model =
    { session : Session }


view =
    Html.text "Settings"

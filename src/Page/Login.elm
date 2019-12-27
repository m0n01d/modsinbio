module Page.Login exposing (..)

import Data.Session exposing (Session)
import Html


type alias Model =
    { session : Session }


update msg model =
    ( model, Cmd.none )


view =
    Html.text "Login"

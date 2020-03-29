module Page.Login exposing (..)

import Data.Session exposing (Session)
import Html
import Http
import RemoteData exposing (RemoteData(..))


type alias Model =
    { session : Session
    , loginResponse : RemoteData Http.Error ()
    , email : String
    }


update msg model =
    ( model, Cmd.none )


view =
    Html.text "Login"

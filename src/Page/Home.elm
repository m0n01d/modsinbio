module Page.Home exposing (..)

import Data.Session exposing (Session)
import Html exposing (Html)
import Route


type alias Model =
    { session : Session }


type Msg
    = NoOp


update msg model =
    ( model, Cmd.none )


view =
    Html.div []
        [ Html.p []
            [ Html.a [ Route.href Route.Login ]
                [ Html.text "login"
                ]
            ]
        , Html.p [] [ Html.a [ Route.href Route.Home ] [ Html.text "home" ] ]
        , Html.p [] [ Html.a [ Route.href Route.Admin ] [ Html.text "My Mods" ] ]
        , Html.p [] [ Html.a [ Route.href Route.Settings ] [ Html.text "settings" ] ]
        ]

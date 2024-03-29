module Route exposing (..)

import Html.Attributes as Attributes
import Url exposing (Url)
import Url.Parser exposing ((</>), (<?>), Parser, map, oneOf, parse, s, string, top)
import Url.Parser.Query as Query


type Route
    = Home
    | Login
    | Admin
    | Settings
    | Authed (Maybe String)
    | Profile String


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map Login (s "login")
        , map Admin (s "app" </> s "mymods")
        , map Settings (s "app" </> s "settings")
        , map Authed (s "app" </> s "authed" <?> Query.string "token")
        , map Profile string
        ]


fromUrl : Url -> Maybe Route
fromUrl =
    parse routeParser


routeToString : Route -> String
routeToString route =
    case route of
        Home ->
            "/"

        Login ->
            "/login"

        Admin ->
            "/app/mymods"

        Settings ->
            "/app/settings"

        Authed payload ->
            payload
                |> Maybe.map ((++) "/app/authed?payload=")
                |> Maybe.withDefault "/login"

        Profile username ->
            String.concat [ "/", username ]


href route =
    Attributes.href (routeToString route)

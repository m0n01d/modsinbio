module Route exposing (..)

import Html.Attributes as Attributes
import Url exposing (Url)
import Url.Parser exposing (Parser, map, oneOf, parse, s, top)


type Route
    = Home
    | Login
    | Admin
    | Settings


routeParser : Parser (Route -> a) a
routeParser =
    oneOf
        [ map Home top
        , map Login (s "login")
        , map Admin (s "mymods")
        , map Settings (s "settings")
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
            "/mymods"

        Settings ->
            "/settings"


href route =
    Attributes.href (routeToString route)

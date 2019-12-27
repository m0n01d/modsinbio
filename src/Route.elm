module Route exposing (..)

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
        , map Admin (s "admin")
        , map Settings (s "settings")
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> parse routeParser

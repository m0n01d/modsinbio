module Network.Api exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Set
import Url.Builder as Builder


apiUrl =
    "http://localhost:8080"


type Fragment
    = Fragment String (List Fragment)


type Document
    = Document String (List String)


type Error
    = Error String



-- CONSTRUCTORS


fragment : String -> List Fragment -> Fragment
fragment body referencedFragments =
    Fragment body referencedFragments


document : String -> List Fragment -> Document
document operation fragments =
    Document operation (flatten fragments)


query session token doc maybeVariables decoder =
    Http.task
        { method = "POST"
        , headers = buildHeaders token
        , url = Builder.crossOrigin apiUrl [ "v1", "graphql" ] []
        , body = Http.jsonBody (buildBody doc maybeVariables)
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


jsonResolver : Decoder a -> Http.Resolver Error a
jsonResolver decoder =
    Http.stringResolver <|
        \response ->
            case response of
                Http.GoodStatus_ _ body ->
                    Decode.decodeString (Decode.field "data" decoder) body
                        |> Result.mapError resultErrorToChangeset

                _ ->
                    Err (Error "")


resultErrorToChangeset : Decode.Error -> Error
resultErrorToChangeset err =
    case err of
        Decode.Field _ err_ ->
            case err_ of
                Decode.Failure msg _ ->
                    Error msg

                _ ->
                    Error "Invalid request"

        _ ->
            Error "Invalid request"


buildHeaders : String -> List Http.Header



--- TODO APITOKEN


buildHeaders token =
    [ Http.header "Authorization" ("Bearer " ++ token)
    ]


buildBody : Document -> Maybe Value -> Value
buildBody doc maybeVariables =
    let
        queryParams =
            serializeDocument doc
    in
    case maybeVariables of
        Nothing ->
            Encode.object
                [ ( "query", Encode.string queryParams ) ]

        Just variables ->
            Encode.object
                [ ( "query", Encode.string queryParams )
                , ( "variables", variables )
                ]


serializeDocument : Document -> String
serializeDocument (Document body fragments) =
    (body :: fragments)
        |> List.map normalize
        |> String.join "\n"


normalize : String -> String
normalize value =
    let
        lines =
            value
                |> String.lines

        firstLine =
            lines
                |> List.head
                |> Maybe.withDefault ""

        tailPadding =
            lines
                |> List.tail
                |> Maybe.withDefault []
                |> List.map String.toList
                |> List.map (countPadding 0)
                |> List.minimum
                |> Maybe.withDefault 0
    in
    lines
        |> List.tail
        |> Maybe.withDefault []
        |> List.map (String.dropLeft tailPadding)
        |> (::) firstLine
        |> String.join "\n"


countPadding : Int -> List Char -> Int
countPadding count list =
    case list of
        [ ' ' ] ->
            count + 1

        ' ' :: tl ->
            countPadding (count + 1) tl

        _ ->
            count


flatten : List Fragment -> List String
flatten fragments =
    let
        toList : Fragment -> List String
        toList f =
            case f of
                Fragment body [] ->
                    [ body ]

                Fragment body referencedFragments ->
                    referencedFragments
                        |> List.map toList
                        |> List.concat
                        |> (::) body
    in
    fragments
        |> List.map toList
        |> List.concat
        |> uniq


uniq : List comparable -> List comparable
uniq list =
    list
        |> Set.fromList
        |> Set.toList

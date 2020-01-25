module Network.SignedUrl exposing (getSignedUrl, resolver)

import Data.User as User
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Url.Builder


getSignedUrl token userId mimeType =
    -- @todo auth token
    Http.task
        { method = "get"
        , headers = []
        , body = Http.emptyBody
        , resolver = Http.stringResolver resolver
        , url =
            Url.Builder.absolute [ "api", "signedUrl" ]
                [ Url.Builder.string "key" (User.idToString userId)
                , Url.Builder.string "type" mimeType
                ]
        , timeout = Nothing
        }


resolver response =
    -- todo move to utils
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata body ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ metadata body ->
            let
                _ =
                    Debug.log "boddyy" body
            in
            case Decode.decodeValue Decode.string (Encode.string body) of
                Ok value ->
                    Ok value

                Err err ->
                    Err (Http.BadBody (Decode.errorToString err))

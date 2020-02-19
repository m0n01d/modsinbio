module Network.SignedUrl exposing (getSignedUrl)

import Data.User as User
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Network.Util as Util
import Url.Builder


getSignedUrl token userId mimeType =
    -- @todo auth token
    Http.task
        { method = "get"
        , headers = []
        , body = Http.emptyBody
        , resolver = Http.stringResolver Util.resolver
        , url =
            Url.Builder.absolute [ "api", "signedUrl" ]
                [ Url.Builder.string "key" (User.idToString userId)
                , Url.Builder.string "type" mimeType
                ]
        , timeout = Nothing
        }

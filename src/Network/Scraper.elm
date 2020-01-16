module Network.Scraper exposing (fetchTitle)

import Http
import Url
import Url.Builder


fetchTitle : (Result Http.Error String -> msg) -> Url.Url -> Cmd msg
fetchTitle onComplete url =
    Http.get
        { url =
            Url.Builder.absolute [ "api", "scrape", "title" ]
                [ Url.Builder.string "url" (Url.toString url)
                ]
        , expect = Http.expectString onComplete
        }

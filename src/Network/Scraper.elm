module Network.Scraper exposing (..)

import Http
import Url
import Url.Builder


type Thing
    = Thing


fetchTitle onComplete url =
    Http.get
        { url =
            Url.Builder.absolute [ "scrape", "title" ]
                [ Url.Builder.string "url" (Url.toString url)
                ]
        , expect = Http.expectString onComplete
        }

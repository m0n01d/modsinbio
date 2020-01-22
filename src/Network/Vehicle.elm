module Network.Vehicle exposing (..)

import Data.Vehicle as Vehicle
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Url.Builder


xapiUrl =
    "https://vpic.nhtsa.dot.gov/api/vehicles/getallmakes?format=json"


x =
    "https://vpic.nhtsa.dot.gov/api/vehicles/getmodelsformakeyear/make/subaru/modelyear/2018?format=json"


apiUrl =
    "https://vpic.nhtsa.dot.gov/api/vehicles"


asJson =
    Url.Builder.string "format" "json"


decodeMakes =
    Decode.succeed identity
        |> Decode.required "Results" (Decode.list Vehicle.decodeMake)


fetchMakes onComplete =
    Http.get
        { url =
            Url.Builder.crossOrigin apiUrl [ "getallmakes" ] [ asJson ]
        , expect = Http.expectJson onComplete decodeMakes
        }


decodeModels =
    Decode.succeed identity
        |> Decode.required "Results" (Decode.list Vehicle.decodeModel)


fetchModels { make, year, onComplete } =
    Http.get
        { url =
            Url.Builder.crossOrigin apiUrl
                [ "getmodelsformakeyear"
                , "make"
                , make
                , "modelyear"
                , year
                ]
                [ asJson ]
        , expect = Http.expectJson onComplete decodeModels
        }

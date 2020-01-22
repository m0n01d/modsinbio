module Data.Vehicle exposing (..)

import Json.Decode as Decode
import Json.Decode.Pipeline as Decode


type VehicleId
    = VehicleId String


type MakeId
    = MakeId Int


type alias Make =
    { id : MakeId
    , name : String
    }


decodeMake =
    Decode.succeed Make
        |> Decode.required "Make_ID" (Decode.int |> Decode.map MakeId)
        |> Decode.required "Make_Name" Decode.string


type ModelId
    = ModelId Int


type alias Model =
    { id : ModelId
    , name : String
    }


decodeModel =
    Decode.succeed Model
        |> Decode.required "Model_ID" (Decode.int |> Decode.map ModelId)
        |> Decode.required "Model_Name" Decode.string


type alias Vehicle =
    { make : Make
    , model : Model
    }

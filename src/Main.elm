module Main exposing (..)

import Browser exposing (Document)
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Page.MyMods as MyMods



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "hi"
    , body =
        [ div []
            [ h1 [] [ text "Your Elm App is workings!" ]
            , MyMods.view
            ]
        ]
    }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = \_ _ _ -> init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = always NoOp
        , onUrlRequest = always NoOp
        }

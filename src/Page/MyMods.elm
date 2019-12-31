module Page.MyMods exposing (..)

import Data.Session exposing (Session)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Network.Scraper as Scraper
import RemoteData exposing (RemoteData(..), WebData)
import Url


type alias Url =
    { url : Url.Url
    , title : String
    , description : String
    }


type alias Model =
    { session : Session
    , links : List Url
    , formIsHidden : Bool
    , suggestedTitle : WebData String
    , newUrl : String
    }


initialModel session =
    { session = session
    , links = []
    , formIsHidden = True
    , suggestedTitle = RemoteData.NotAsked
    , newUrl = ""
    }


view model =
    Html.div []
        [ Html.div []
            [ Html.p [] [ Html.text "my mods" ] ]
        , Html.div [ Attributes.class "flex" ]
            [ Html.div [ Attributes.class "flex-1" ]
                [ Html.button
                    [ Attributes.class "h-8 px-6 text-2xl rounded-sm border"
                    , Events.onClick ToggleNewLinkForm
                    ]
                    [ Html.text "Add new link" ]
                , Html.form [ Attributes.classList [ ( "hidden", model.formIsHidden ) ] ]
                    [ Html.div []
                        [ Html.label [] [ Html.text "Url" ]
                        , Html.input
                            [ Attributes.class "border ml-2"
                            , Attributes.placeholder "https://fastcars.com"
                            , Events.onInput SetNewUrl
                            ]
                            []
                        ]
                    , Html.div []
                        [ Html.label [] [ Html.text "Title" ]
                        , Html.input
                            [ Attributes.class "border ml-2"
                            , Attributes.placeholder "Go fast parts"
                            ]
                            []
                        , case model.suggestedTitle of
                            Loading ->
                                Html.text "..."

                            Success t ->
                                Html.span [] [ Html.text t ]

                            _ ->
                                Html.text ""
                        ]
                    , Html.div []
                        [ Html.label [] [ Html.text "Description" ]
                        , Html.input
                            [ Attributes.class "border ml-2"
                            , Attributes.placeholder "Shoutout to @rallitek"
                            ]
                            []
                        ]
                    ]
                , Html.div [ Attributes.class "" ]
                    [ Html.text "my links"
                    , Html.ul []
                        (model.links
                            |> List.map viewLink
                        )
                    ]
                ]
            , Html.div [ Attributes.class "flex-1" ] [ Html.text "preview" ]
            ]
        ]


viewLink url =
    Html.li []
        [ Html.text url.title
        ]


x =
    "https://www.fastwrx.com/collections/shift-knobs/products/cobb-6-speed-shift-knob"


type Msg
    = ToggleNewLinkForm
    | SetNewUrl String
    | FetchTitleResponse (WebData String)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleNewLinkForm ->
            ( { model | formIsHidden = not model.formIsHidden }
            , Cmd.none
            )

        SetNewUrl url ->
            ( { model | newUrl = url, suggestedTitle = Loading }
            , Url.fromString url
                |> Maybe.map (Scraper.fetchTitle (RemoteData.fromResult >> FetchTitleResponse))
                |> Maybe.withDefault Cmd.none
            )

        FetchTitleResponse res ->
            ( { model | suggestedTitle = res }, Cmd.none )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

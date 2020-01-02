module Page.MyMods exposing (..)

import Data.Session exposing (Session)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Network.Scraper as Scraper
import RemoteData exposing (RemoteData(..), WebData)
import Url


type alias Mod =
    { url : Url.Url
    , title : String
    , description : String
    }


type alias Model =
    { session : Session
    , mods : Mods
    }


type alias Mods =
    Dict String ModSection


type alias ModSection =
    { formIsHidden : Bool
    , suggestedTitle : WebData String
    , newUrl : String
    , newTitle : String
    , mods : List Mod
    }


newModSection =
    { formIsHidden = True
    , suggestedTitle = NotAsked
    , newUrl = ""
    , newTitle = ""
    , mods = []
    }


initialMods =
    [ ( "Engine", newModSection )
    , ( "Exterior", newModSection )
    , ( "Interior", newModSection )
    , ( "Suspension", newModSection )
    , ( "Wheels", newModSection )
    , ( "Misc", newModSection )
    ]


initialModel session =
    { session = session
    , mods = Dict.fromList initialMods
    }


view model =
    Html.div []
        [ Html.div []
            [ Html.p [ Attributes.class "capitalize text-center" ]
                [ Html.text "my mods" ]
            ]
        , Html.div [ Attributes.class "flex md:flex-row flex-col" ]
            [ Html.div [ Attributes.class "flex-1" ]
                [ Html.div []
                    [ Html.ul
                        []
                        (model.mods
                            |> Dict.foldr
                                (\k v acc ->
                                    Html.li []
                                        [ Html.div [ Attributes.class "flex w-full px-2" ]
                                            [ Html.p [ Attributes.class "font-semibold mr-auto" ]
                                                [ Html.text k ]
                                            , Html.button
                                                [ Attributes.type_ "button"
                                                , Attributes.class "py-2 rounded-sm text-sm"
                                                , Events.onClick <| ToggleNewLinkForm k
                                                ]
                                                [ Html.text "Add Link" ]
                                            ]
                                        , viewNewLink k v
                                        , Html.div []
                                            (v.mods
                                                |> List.map viewLink
                                            )
                                        ]
                                        :: acc
                                )
                                []
                        )
                    ]
                ]
            , Html.div [ Attributes.class "flex-1" ] [ Html.text "preview" ]
            ]
        ]


viewNewLink : String -> ModSection -> Html Msg
viewNewLink k section =
    Html.form
        [ Attributes.classList
            [ ( "hidden"
              , section.formIsHidden
              )
            , ( "px-2 mb-4 py-2", True )
            ]
        ]
        [ Html.div [ Attributes.class "flex items-center mb-1" ]
            [ Html.label [ Attributes.class "font-medium text-sm" ] [ Html.text "Url:" ]
            , Html.input
                [ Attributes.class "border ml-2 flex-1 px-1 rounded-sm"
                , Attributes.placeholder "https://fastcars.com"
                , Events.onInput SetNewUrl
                ]
                []
            ]
        , Html.div [ Attributes.class "flex items-center mb-1" ]
            [ Html.label [ Attributes.class "font-medium text-sm" ] [ Html.text "Title:" ]
            , Html.input
                [ Attributes.class "border ml-2 flex-1 px-1 rounded-sm"
                , Attributes.placeholder "Go fast parts"
                , Events.onInput SetNewTitle
                , Attributes.value section.newTitle
                ]
                []
            , case section.suggestedTitle of
                Loading ->
                    Html.text "..."

                Success title ->
                    Html.button
                        [ Events.onClick <|
                            UseSuggestedTitle title
                        , Attributes.type_ "button"
                        , Attributes.class "mr-1 text-sm "
                        ]
                        [ Html.text title ]

                _ ->
                    Html.text ""
            ]
        , Html.div []
            [ Html.label [ Attributes.class "mb-1 font-semibold text-sm" ] [ Html.text "Description:" ]
            , Html.textarea
                [ Attributes.placeholder "For shoutouts and things"
                , Attributes.class "border rounded-sm w-full px-2 py-1"
                ]
                []
            ]
        , Html.button [ Attributes.class "px-4 py-2 font-medium text-center rounded-sm border" ] [ Html.text "Save" ]
        ]


viewLink url =
    Html.li []
        [ Html.text url.title
        ]


x =
    "https://www.fastwrx.com/collections/shift-knobs/products/cobb-6-speed-shift-knob"


type Msg
    = ToggleNewLinkForm String
    | SetNewUrl String
    | FetchTitleResponse (WebData String)
    | UseSuggestedTitle String
    | SetNewTitle String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleNewLinkForm sectionTitle ->
            --todo
            ( { model
                | mods =
                    Dict.update sectionTitle
                        (\v ->
                            case v of
                                Just section ->
                                    Just { section | formIsHidden = not section.formIsHidden }

                                Nothing ->
                                    Nothing
                        )
                        model.mods
              }
            , Cmd.none
            )

        -- SetNewUrl url ->
        --     -- todo debounce fetching suggested title
        --     ( { model | newUrl = url, suggestedTitle = Loading }
        --     , Url.fromString url
        --         |> Maybe.map (Scraper.fetchTitle (RemoteData.fromResult >> FetchTitleResponse))
        --         |> Maybe.withDefault Cmd.none
        --     )
        FetchTitleResponse res ->
            ( model
            , Cmd.none
            )

        UseSuggestedTitle title ->
            update (SetNewTitle title)
                model

        SetNewTitle title ->
            ( model
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

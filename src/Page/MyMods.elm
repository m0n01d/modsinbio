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
import Process
import RemoteData exposing (RemoteData(..), WebData)
import Task
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
            [ Html.p [] [ Html.text "My car" ]
            , Html.text "2018 Subaru WRX Premium"
            ]
        , Html.div []
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
                , Events.onInput <| SetNewUrl k
                , Attributes.value section.newUrl
                ]
                []
            ]
        , Html.div [ Attributes.class " mb-1" ]
            [ Html.div [ Attributes.class "flex items-center" ]
                [ Html.label [ Attributes.class "font-medium text-sm" ]
                    [ Html.text "Title:" ]
                , Html.input
                    [ Attributes.class "border ml-2 flex-1 px-1 rounded-sm"
                    , Attributes.placeholder "Go fast parts"
                    , Events.onInput <| SetNewTitle k
                    , Attributes.disabled (RemoteData.isNotAsked section.suggestedTitle)
                    , section.suggestedTitle
                        |> RemoteData.map Attributes.value
                        |> RemoteData.withDefault (Attributes.name "")
                    , Attributes.placeholder <|
                        -- consider using label to show fetching
                        if RemoteData.isLoading section.suggestedTitle then
                            "Fetching title..."

                        else
                            ""
                    ]
                    []
                ]
            ]
        , Html.div []
            [ Html.label [ Attributes.class "mb-1 font-semibold text-sm" ]
                [ Html.text "Description:" ]
            , Html.textarea
                [ Attributes.placeholder "For shoutouts and things"
                , Attributes.class "border rounded-sm w-full px-2 py-1"
                ]
                []
            ]
        , Html.button
            [ Attributes.class "px-4 py-2 font-medium text-center rounded-sm border"
            ]
            [ Html.text "Save" ]
        ]


viewLink url =
    Html.li []
        [ Html.text url.title
        ]


type alias SectionTitle =
    String


type Msg
    = ToggleNewLinkForm SectionTitle
    | SetNewUrl SectionTitle String
    | FetchTitleResponse SectionTitle (WebData String)
    | UseSuggestedTitle SectionTitle String
    | SetNewTitle SectionTitle String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleNewLinkForm sectionTitle ->
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

        SetNewUrl sectionTitle url ->
            -- todo debounce fetching suggested title
            ( { model
                | mods =
                    Dict.update sectionTitle
                        (\v ->
                            case v of
                                Just section ->
                                    Just { section | newUrl = url, suggestedTitle = Loading }

                                Nothing ->
                                    Nothing
                        )
                        model.mods
              }
            , Url.fromString url
                |> Maybe.map
                    (Scraper.fetchTitle
                        (RemoteData.fromResult
                            >> FetchTitleResponse sectionTitle
                        )
                    )
                |> Maybe.withDefault
                    (delay 0 (FetchTitleResponse sectionTitle (Success "")))
            )

        FetchTitleResponse sectionTitle res ->
            ( { model
                | mods =
                    Dict.update sectionTitle
                        (\v ->
                            case v of
                                Just section ->
                                    Just { section | suggestedTitle = res }

                                Nothing ->
                                    Nothing
                        )
                        model.mods
              }
            , Cmd.none
            )

        UseSuggestedTitle sectionTitle title ->
            update (SetNewTitle sectionTitle title)
                { model
                    | mods =
                        Dict.update sectionTitle
                            (\v ->
                                case v of
                                    Just section ->
                                        Just { section | suggestedTitle = NotAsked }

                                    Nothing ->
                                        Nothing
                            )
                            model.mods
                }

        SetNewTitle sectionTitle title ->
            ( { model
                | mods =
                    Dict.update sectionTitle
                        (\v ->
                            case v of
                                Just section ->
                                    Just { section | newTitle = title }

                                Nothing ->
                                    Nothing
                        )
                        model.mods
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


delay n msg =
    Process.sleep n
        |> Task.perform (always msg)

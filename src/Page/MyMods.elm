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
    { id : String
    , url : Url.Url
    , title : String
    , description : String
    , panel : Maybe MorePanel
    }


type MorePanel
    = DeletionPanel
    | AnalyticsPanel


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
    , newDescription : String
    , mods : List Mod
    , savingState : WebData ()
    }


newModSection =
    { formIsHidden = True
    , suggestedTitle = NotAsked
    , newUrl = ""
    , newTitle = ""
    , newDescription = ""
    , mods = []
    , savingState = NotAsked
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
        , Html.div [ Attributes.class "flex md:flex-row flex-col" ]
            [ Html.div [ Attributes.class "flex-1" ]
                [ Html.div [ Attributes.class "mt-4" ]
                    [ Html.div []
                        [ Html.p [ Attributes.class "capitalize text-center" ]
                            [ Html.text "my mods" ]
                        ]
                    , Html.ul
                        []
                        (model.mods
                            |> Dict.foldr
                                (\k v acc ->
                                    Html.li []
                                        [ Html.div [ Attributes.class "flex items-center w-full px-1 py-1 border-b border-gray-100" ]
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
                                        , Html.div [ Attributes.class "px-1 py-px bg-gray-200" ]
                                            (if True then
                                                v.mods
                                                    |> List.map (viewLink k)

                                             else
                                                v.mods
                                                    |> List.take 3
                                                    |> List.map (viewLink k)
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
viewNewLink sectionTitle section =
    Html.form
        [ Attributes.classList
            [ ( "hidden"
              , section.formIsHidden
              )
            , ( "px-2 mb-4 py-2", True )
            ]
        , Events.onSubmit <| AddLink sectionTitle
        , Attributes.disabled <| RemoteData.isLoading section.savingState
        ]
        [ Html.div [ Attributes.class "flex items-center mb-1" ]
            [ Html.label [ Attributes.class "font-medium text-sm" ]
                [ Html.text "Url:" ]
            , Html.input
                [ Attributes.class "border ml-2 flex-1 px-1 rounded-sm"
                , Attributes.placeholder "https://fastcars.com"
                , Events.onInput <| SetNewUrl sectionTitle
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
                    , Events.onInput <| SetNewTitle sectionTitle
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
                , Attributes.value section.newDescription
                , Events.onInput <| SetNewDescription sectionTitle
                ]
                [ Html.text section.newDescription ]
            ]
        , Html.button
            [ Attributes.class "px-4 py-2 font-medium text-center rounded-sm border"
            , Attributes.classList [ ( "cursor-not-allowed opacity-50", RemoteData.isLoading section.savingState ) ]
            , Attributes.disabled <| RemoteData.isLoading section.savingState
            ]
            [ Html.text "Save" ]
        ]


viewLink sectionTitle link =
    Html.li []
        [ Html.div [ Attributes.class "bg-white my-1 px-2 py-1 rounded-sm" ]
            [ Html.p [] [ Html.text link.title ]
            , Html.p [ Attributes.class "truncate text-gray-700 text-sm mt-px" ]
                [ Html.a
                    [ Attributes.href <| Url.toString link.url
                    , Attributes.rel "noopener"
                    , Attributes.target "_blank"
                    , Attributes.class "text-blue-500"
                    ]
                    [ Html.text <| Url.toString link.url ]
                ]
            , Html.div
                [ Attributes.class "text-xs mt-1 py-1 flex items-center"
                ]
                [ Html.label [ Attributes.class "mx-2" ]
                    [ Html.text "Active:"
                    , Html.input
                        [ Attributes.type_ "checkbox"
                        , Attributes.name <| String.join " " [ link.title, "enabled" ]
                        , Attributes.checked True
                        ]
                        []
                    ]
                , Html.div [ Attributes.class "ml-auto" ]
                    [ Html.button
                        [ Attributes.class "mx-1"
                        , Events.onClick <| OpenPanel sectionTitle link.id DeletionPanel
                        ]
                        [ Html.text "Delete" ]
                    , Html.button [ Attributes.class "mx-1" ] [ Html.text "Analytics" ]
                    ]
                ]
            , link.panel
                |> Maybe.map (viewMorePanel sectionTitle link)
                |> Maybe.withDefault (Html.text "")
            ]
        ]


viewMorePanel sectionTitle link panel =
    case panel of
        DeletionPanel ->
            Html.div [ Attributes.class "text-center" ]
                [ Html.p
                    [ Attributes.class "relative bg-gray-200 "
                    ]
                    [ Html.text "Delete"
                    , Html.button
                        [ Attributes.class "float-right px-2 py-1 -mt-1 monospace"
                        , Events.onClick (ClosePanel sectionTitle link.id)
                        ]
                        [ Html.text "X" ]
                    ]
                , Html.div [ Attributes.class " py-4 px-2" ]
                    [ Html.p [] [ Html.text <| String.concat [ "Are you sure you want to permanently delete: \"", link.title, "\"?" ] ]
                    , Html.button
                        [ Attributes.class "mt-2 px-4 py-2 font-medium text-center rounded-sm border mr-4"
                        , Events.onClick (ClosePanel sectionTitle link.id)
                        ]
                        [ Html.text "No" ]
                    , Html.button
                        [ Attributes.class "mt-2 px-4 py-2 font-medium text-center rounded-sm border border-red-800 bg-red-500 text-white"
                        ]
                        [ Html.text "Yes" ]
                    ]
                ]

        AnalyticsPanel ->
            Html.text "analytic"


type alias SectionTitle =
    String


type Msg
    = ToggleNewLinkForm SectionTitle
    | SetNewUrl SectionTitle String
    | FetchTitleResponse SectionTitle (WebData String)
    | UseSuggestedTitle SectionTitle String
    | SetNewTitle SectionTitle String
    | SetNewDescription SectionTitle String
    | AddLink SectionTitle
    | AddLinkResponse SectionTitle (WebData ())
    | OpenPanel SectionTitle String MorePanel
    | ClosePanel SectionTitle String
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClosePanel sectionTitle id ->
            ( { model
                | mods =
                    Dict.update sectionTitle
                        (\v ->
                            case v of
                                Just section ->
                                    Just
                                        { section
                                            | mods =
                                                section.mods
                                                    |> List.map
                                                        (\m ->
                                                            --todo
                                                            if m.id == id then
                                                                { m | panel = Nothing }

                                                            else
                                                                m
                                                        )
                                        }

                                Nothing ->
                                    Nothing
                        )
                        model.mods
              }
            , Cmd.none
            )

        OpenPanel sectionTitle id panel ->
            ( { model
                | mods =
                    Dict.update sectionTitle
                        (\v ->
                            case v of
                                Just section ->
                                    Just
                                        { section
                                            | mods =
                                                section.mods
                                                    |> List.map
                                                        (\m ->
                                                            --todo
                                                            if m.id == id then
                                                                { m | panel = Just panel }

                                                            else
                                                                m
                                                        )
                                        }

                                Nothing ->
                                    Nothing
                        )
                        model.mods
              }
            , Cmd.none
            )

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
                                    Just
                                        { section
                                            | newUrl = url
                                            , suggestedTitle = Loading
                                            , savingState = NotAsked
                                        }

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

        FetchTitleResponse sectionTitle (Success title) ->
            ( { model
                | mods =
                    Dict.update sectionTitle
                        (\v ->
                            case v of
                                Just section ->
                                    Just
                                        { section
                                            | suggestedTitle = Success title
                                            , newTitle = title
                                        }

                                Nothing ->
                                    Nothing
                        )
                        model.mods
              }
            , Cmd.none
            )

        FetchTitleResponse sectionTitle (Failure why) ->
            ( model, Cmd.none )

        FetchTitleResponse sectionTitle _ ->
            ( model, Cmd.none )

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

        SetNewDescription sectionTitle description ->
            ( { model
                | mods =
                    Dict.update sectionTitle
                        (\v ->
                            case v of
                                Just section ->
                                    Just { section | newDescription = description }

                                Nothing ->
                                    Nothing
                        )
                        model.mods
              }
            , Cmd.none
            )

        AddLink sectionTitle ->
            --todo
            -- validate fields
            ( { model
                | mods =
                    Dict.update sectionTitle
                        (\v ->
                            case v of
                                Just section ->
                                    Just { section | savingState = Loading }

                                Nothing ->
                                    Nothing
                        )
                        model.mods
              }
            , delay 1000 (AddLinkResponse sectionTitle (Success ()))
            )

        AddLinkResponse sectionTitle (Success ()) ->
            case Dict.get sectionTitle model.mods of
                Just section ->
                    case Url.fromString section.newUrl of
                        Just url ->
                            let
                                newUrl =
                                    { url = url
                                    , description = section.newDescription
                                    , title = section.newTitle
                                    , panel = Nothing
                                    , id = Url.toString url
                                    }
                            in
                            ( { model
                                | mods =
                                    Dict.update sectionTitle
                                        (\v ->
                                            case v of
                                                Just sect ->
                                                    Just { newModSection | mods = newUrl :: section.mods }

                                                Nothing ->
                                                    Nothing
                                        )
                                        model.mods
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        AddLinkResponse sectionTitle _ ->
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


delay n msg =
    Process.sleep n
        |> Task.perform (always msg)

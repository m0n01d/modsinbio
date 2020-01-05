module Page.MyMods exposing (..)

import Data.Session exposing (Session)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode exposing (Value)
import Network.Scraper as Scraper
import Process
import RemoteData exposing (RemoteData(..), WebData)
import Task
import Url


type alias Mod =
    { id : String
    , url : Maybe Url.Url
    , urlString : String
    , title : String
    , description : String
    , panel : Maybe MorePanel
    }


decodeToUrl =
    Decode.string
        |> Decode.map Url.fromString


decodeMod =
    Decode.succeed Mod
        |> Decode.required "id" Decode.string
        |> Decode.custom (Decode.field "urlString" decodeToUrl)
        |> Decode.required "urlString" Decode.string
        |> Decode.required "title" Decode.string
        |> Decode.required "description" Decode.string
        |> Decode.hardcoded Nothing


type MorePanel
    = DeletionPanel
    | AnalyticsPanel


type alias Model =
    { isNewCategoryFormVisible : Bool -- refactor to maybe?
    , mods : Mods
    , newCategoryName : String
    , session : Session
    }


type alias NewLink =
    { url : String
    , description : String
    , title : String
    }


encodeNewLink { url, description, title } =
    Encode.object
        [ ( "url", Encode.string url )
        , ( "description", Encode.string description )
        , ( "title", Encode.string title )
        ]


type alias Mods =
    Dict CategoryId ModCategory


type alias CategoryId =
    Int


type alias ModCategory =
    { formIsHidden : Bool
    , id : CategoryId
    , isEditingCategoryTitle : Bool
    , mods : List Mod
    , newDescription : String
    , newTitle : String -- new link title -- todo move to form
    , newTitle_ : String -- new category title
    , newUrl : String
    , order : Int
    , savingState : WebData ()
    , suggestedTitle : WebData String
    , title : String
    }


newModCategory title id =
    { formIsHidden = True
    , suggestedTitle = NotAsked
    , newUrl = ""
    , newTitle = ""
    , newDescription = ""
    , mods = []
    , savingState = NotAsked
    , isEditingCategoryTitle = False
    , id = id
    , title = title
    , newTitle_ = title
    , order = id -- order
    }


initialMods =
    [ newModCategory "Engine"
    , newModCategory "Exterior"
    , newModCategory "Interior"
    , newModCategory "Suspension"
    , newModCategory "Wheels"
    , newModCategory "Misc"
    ]
        |> List.indexedMap (\i x -> ( i, x i ))


initialModel session =
    { session = session
    , mods = Dict.fromList initialMods
    , isNewCategoryFormVisible = False
    , newCategoryName = ""
    }


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ Attributes.class "md:px-8 mb-2" ]
            [ Html.p []
                [ Html.text "My car:"
                , Html.span [ Attributes.class "ml-1" ] [ Html.text "2018 Subaru WRX Premium" ]
                ]
            ]
        , Html.div [ Attributes.class "flex md:flex-row flex-col" ]
            [ Html.div [ Attributes.class "flex-1" ]
                [ Html.div [ Attributes.class "mt-4 md:px-8" ]
                    [ Html.div []
                        [ Html.p [ Attributes.class "capitalize text-center" ]
                            [ Html.text "my mods" ]
                        ]
                    , Html.ul
                        []
                        (model.mods
                            |> Dict.foldr
                                (\categoryId category acc ->
                                    Html.li []
                                        [ Html.div [ Attributes.class "flex items-center w-full px-1 py-1 " ]
                                            [ Html.p
                                                [ Attributes.class "font-semibold mr-auto"
                                                , Attributes.classList [ ( "hidden", category.isEditingCategoryTitle ) ]
                                                ]
                                                [ Html.text category.title
                                                , Html.button
                                                    [ Attributes.class "text-xs ml-2"
                                                    , Events.onClick <| ToggleEditCategory category.id
                                                    ]
                                                    [ Html.text "Edit" ]
                                                ]
                                            , Html.div
                                                [ Attributes.class " mr-auto"
                                                , Attributes.classList
                                                    [ ( "hidden", not category.isEditingCategoryTitle )
                                                    , ( "font-semibold block", True )
                                                    ]
                                                ]
                                                [ Html.input
                                                    [ Attributes.value category.newTitle_
                                                    , Attributes.class "border px-1"
                                                    , Events.onInput <| SetNewCategoryTitle category.id
                                                    ]
                                                    []
                                                , Html.button
                                                    [ Attributes.class "ml-1 text-xs"
                                                    , Events.onClick <| SaveNewCategoryId category.id
                                                    ]
                                                    [ Html.text "Save" ]
                                                ]
                                            , Html.button
                                                [ Attributes.type_ "button"
                                                , Attributes.class "py-2 rounded-sm text-sm"
                                                , Events.onClick <| ToggleNewLinkForm category.id
                                                ]
                                                [ Html.text "Add Link" ]
                                            ]
                                        , viewNewLinkForm category
                                        , Html.div [ Attributes.class "px-1 py-px bg-gray-200 rounded-sm" ]
                                            (if True then
                                                category.mods
                                                    |> List.map (viewLink category.id)

                                             else
                                                category.mods
                                                    |> List.take 3
                                                    |> List.map (viewLink category.id)
                                            )
                                        ]
                                        :: acc
                                )
                                []
                        )
                    , Html.button
                        [ Attributes.class "px-4 py-1 font-medium text-center rounded-sm border my-3 "
                        , Events.onClick ToggleNewCategoryForm
                        ]
                        [ Html.text "+ Add New Category" ]
                    , Html.form
                        [ Attributes.classList
                            [ ( "hidden"
                              , not model.isNewCategoryFormVisible
                              )
                            ]
                        , Events.onSubmit AddNewCategory
                        ]
                        [ Html.label [ Attributes.class "font-medium" ]
                            [ Html.text "Name:"
                            , Html.input
                                [ Attributes.class "ml-1 px-2 border py-1 mx-1"
                                , Attributes.placeholder "Things and stuff"
                                , Events.onInput SetNewCategoryName
                                , Attributes.value model.newCategoryName
                                ]
                                []
                            ]
                        , Html.button [ Attributes.class "px-4 py-1 font-medium text-center rounded-sm border" ] [ Html.text "Save" ]
                        ]
                    ]
                ]
            , Html.div [ Attributes.class "flex-1" ]
                [ viewPreview model ]
            ]
        ]


viewNewLinkForm : ModCategory -> Html Msg
viewNewLinkForm category =
    let
        categoryId =
            category.id
    in
    Html.form
        [ Attributes.classList
            [ ( "hidden"
              , category.formIsHidden
              )
            , ( "px-2 mb-4 py-2", True )
            ]
        , Events.onSubmit <| AddLink categoryId
        , Attributes.disabled <| RemoteData.isLoading category.savingState
        ]
        [ Html.div [ Attributes.class "flex items-center mb-1" ]
            [ Html.label [ Attributes.class "font-medium text-sm" ]
                [ Html.text "Url:" ]
            , Html.input
                [ Attributes.class "border ml-2 flex-1 px-1 rounded-sm"
                , Attributes.placeholder "https://www.flyinmiata.com/custom-turbo-system-na8-chassis.html"
                , Events.onInput <| SetNewUrl categoryId
                , Attributes.value category.newUrl
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
                    , Events.onInput <| SetNewTitle categoryId
                    , Attributes.disabled (RemoteData.isNotAsked category.suggestedTitle)
                    , category.suggestedTitle
                        |> RemoteData.map Attributes.value
                        |> RemoteData.withDefault (Attributes.name "")
                    , Attributes.placeholder <|
                        -- consider using label to show fetching
                        if RemoteData.isLoading category.suggestedTitle then
                            "Fetching title..."

                        else
                            "Turbo noises"
                    ]
                    []
                ]
            ]
        , Html.div []
            [ Html.label [ Attributes.class "mb-1 font-semibold text-sm" ]
                [ Html.text "Description:" ]
            , Html.textarea
                [ Attributes.placeholder "Tell everyone how awesome your setup is"
                , Attributes.class "border rounded-sm w-full px-2 py-1"
                , Attributes.value category.newDescription
                , Events.onInput <| SetNewDescription categoryId
                ]
                [ Html.text category.newDescription ]
            ]
        , Html.button
            [ Attributes.class "px-4 py-2 font-medium text-center rounded-sm border"
            , Attributes.classList [ ( "cursor-not-allowed opacity-50", RemoteData.isLoading category.savingState ) ]
            , Attributes.disabled <| RemoteData.isLoading category.savingState
            ]
            [ Html.text "Save" ]
        ]


viewLink categoryId link =
    Html.li []
        [ Html.div [ Attributes.class "bg-white my-1 px-2 py-1 rounded-sm" ]
            [ Html.p [] [ Html.text link.title ]
            , Html.p [ Attributes.class "truncate text-gray-700 text-sm mt-px" ]
                [ Html.a
                    [ Attributes.href link.urlString
                    , Attributes.rel "noopener"
                    , Attributes.target "_blank"
                    , Attributes.class "text-blue-500"
                    ]
                    [ Html.text link.urlString ]
                ]
            , Html.div
                [ Attributes.class "text-xs mt-1 py-1 flex items-center"
                ]
                [ Html.label [ Attributes.class "mx-2 flex items-center" ]
                    [ Html.text "Active:"
                    , Html.input
                        [ Attributes.type_ "checkbox"
                        , Attributes.name <| String.join " " [ link.title, "enabled" ]
                        , Attributes.checked True
                        , Attributes.class "ml-1"
                        ]
                        []
                    ]
                , Html.div [ Attributes.class "ml-auto" ]
                    [ Html.button
                        [ Attributes.class "mx-1"
                        , Events.onClick <| OpenPanel categoryId link.id DeletionPanel
                        ]
                        [ Html.text "Delete" ]
                    , Html.button [ Attributes.class "mx-1" ] [ Html.text "Analytics" ]
                    ]
                ]
            , link.panel
                |> Maybe.map (viewMorePanel { link = link, onClickClose = ClosePanel categoryId link.id })
                |> Maybe.withDefault (Html.text "")
            ]
        ]


viewMorePanel { onClickClose, link } panel =
    case panel of
        DeletionPanel ->
            Html.div [ Attributes.class "text-center" ]
                [ Html.p
                    [ Attributes.class "relative bg-gray-200 "
                    ]
                    [ Html.text "Delete"
                    , Html.button
                        [ Attributes.class "float-right px-2 py-1 -mt-1 monospace"
                        , Events.onClick onClickClose
                        ]
                        [ Html.text "X" ]
                    ]
                , Html.div [ Attributes.class " py-4 px-2" ]
                    [ Html.p [] [ Html.text <| String.concat [ "Are you sure you want to permanently delete: \"", link.title, "\"?" ] ]
                    , Html.button
                        [ Attributes.class "mt-2 px-4 py-2 font-medium text-center rounded-sm border mr-4"
                        , Events.onClick onClickClose
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


viewPreview model =
    let
        categoryView =
            Html.div [ Attributes.class "my-4" ]
                [ Html.p [ Attributes.class "font-semibold text-sm" ]
                    [ Html.text "Engine" ]
                , Html.ul []
                    [ viewPreviewLink
                    , viewPreviewLink
                    , viewPreviewLink
                    ]
                , Html.button [ Attributes.class "text-center block w-full text-gray-700" ]
                    [ Html.text "+ View all" ]
                ]
    in
    Html.div
        [ Attributes.class "flex flex-col justify-center items-center bg-center bg-contain bg-no-repeat"

        -- , Attributes.style "background-image" "url(/images/iphone.png)"
        ]
        [ Html.div
            [ Attributes.style "width" "320px"
            , Attributes.style "max-height" "529px"

            -- , Attributes.style "transform" "scale(0.75)"
            , Attributes.class " overflow-y-scroll border-2 border-black rounded"
            ]
            [ Html.div [ Attributes.class "bg-white" ]
                [ Html.div [ Attributes.class "px-2" ]
                    [ Html.div
                        [ Attributes.style "background-image" <|
                            String.concat
                                [ "url("
                                , "https://www.placecage.com/300/300"
                                , ")"
                                ]
                        , Attributes.class "bg-contain bg-center bg-no-repeat w-20 h-20 mx-auto mt-8 rounded-sm"
                        ]
                        []
                    , Html.h1 [ Attributes.class "text-center font-medium text-lg mt-2" ]
                        [ Html.text "@dwrxht" ]
                    , Html.p [] [ Html.text "2018 wrx premium" ]
                    , Html.p [] [ Html.text "bio?" ]
                    , categoryView
                    , categoryView
                    , categoryView
                    ]
                ]
            ]
        ]


viewPreviewLink =
    Html.li []
        [ Html.div [ Attributes.class "my-3 px-1" ]
            [ Html.div []
                [ Html.a
                    [ Attributes.class "group text-sm md:text-base leading-tight text-center px-2 py-3 border border-green-600 mt-2 block rounded-sm bg-green-500 text-white hover:bg-white hover:text-green-500"
                    , Attributes.href "https://www.fastwrx.com/collections/shift-knobs/products/cobb-6-speed-shift-knob"
                    ]
                    [ Html.text "COBB 6-Speed Shift Knob | FastWRX.com" ]
                , Html.p
                    [ Attributes.class "truncate text-xs text-gray-600 px-px py-1 hidden" --hidden for now
                    ]
                    [ Html.text "https://www.fastwrx.com/collections/shift-knobs/products/cobb-6-speed-shift-knob" ]
                ]
            , Html.button [ Attributes.class "block bg-gray-300 text-gray-800 w-full text-lg font-bold monospace mt-px" ]
                [ Html.text "···"
                ]
            , Html.div
                [ Attributes.class "border mt-0 px-1 py-2 text-sm text-gray-900"
                , Attributes.classList [ ( "hidden", True ) ]
                ]
                [ Html.text "Here I am adding a comment or whatever. Shoutout to my boy @overlandwrx for the hook up on these sick parts"
                ]
            ]
        ]


type Msg
    = ToggleNewLinkForm CategoryId
    | SetNewUrl CategoryId String
    | FetchTitleResponse CategoryId (WebData String)
    | UseSuggestedTitle CategoryId String
    | SetNewTitle CategoryId String
    | SetNewDescription CategoryId String
    | AddLink CategoryId
    | AddLinkResponse CategoryId (WebData Mod)
    | OpenPanel CategoryId String MorePanel
    | ClosePanel CategoryId String
    | ToggleEditCategory CategoryId
    | SetNewCategoryTitle CategoryId String
    | SaveNewCategoryId CategoryId
    | SaveNewCategoryIdResponse (Result Http.Error String)
    | ToggleNewCategoryForm
    | AddNewCategory
    | SetNewCategoryName String
      -- | SaveUpdatedTitle CategoryId
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetNewCategoryName name ->
            ( { model | newCategoryName = name }, Cmd.none )

        AddNewCategory ->
            let
                n =
                    model.mods
                        |> Dict.toList
                        |> List.length

                id =
                    String.fromInt n
            in
            ( { model
                | mods = Dict.insert n (newModCategory model.newCategoryName n) model.mods
                , isNewCategoryFormVisible = False
                , newCategoryName = ""
              }
            , Cmd.none
            )

        ToggleNewCategoryForm ->
            ( { model | isNewCategoryFormVisible = not model.isNewCategoryFormVisible }, Cmd.none )

        SaveNewCategoryId categoryId ->
            let
                updatedMods =
                    Dict.update categoryId
                        (\v ->
                            case v of
                                Just category ->
                                    Just
                                        { category
                                            | formIsHidden = True
                                            , isEditingCategoryTitle = False
                                            , title = category.newTitle_
                                        }

                                Nothing ->
                                    Nothing
                        )
                        model.mods
            in
            ( { model
                | mods =
                    updatedMods
              }
            , case Dict.get categoryId updatedMods of
                Just category ->
                    Http.post
                        { url = "/api/save"
                        , body =
                            Http.jsonBody <|
                                Encode.object
                                    [ ( "tag", Encode.string "SaveNewCategoryId" )
                                    , ( "payload", Encode.string category.newTitle_ )
                                    ]
                        , expect = Http.expectString SaveNewCategoryIdResponse
                        }

                Nothing ->
                    Cmd.none
            )

        SaveNewCategoryIdResponse res ->
            let
                _ =
                    Debug.log "res" res
            in
            ( model, Cmd.none )

        SetNewCategoryTitle categoryId title ->
            ( { model
                | mods =
                    Dict.update categoryId
                        (\v ->
                            case v of
                                Just category ->
                                    Just
                                        { category
                                            | formIsHidden = True
                                            , newTitle_ = title
                                        }

                                Nothing ->
                                    Nothing
                        )
                        model.mods
              }
            , Cmd.none
            )

        ToggleEditCategory categoryId ->
            ( { model
                | mods =
                    Dict.update categoryId
                        (\v ->
                            case v of
                                Just category ->
                                    Just
                                        { category
                                            | isEditingCategoryTitle = not category.isEditingCategoryTitle
                                            , formIsHidden = True
                                        }

                                Nothing ->
                                    Nothing
                        )
                        model.mods
              }
            , Cmd.none
            )

        ClosePanel categoryId id ->
            ( { model
                | mods =
                    Dict.update categoryId
                        (\v ->
                            case v of
                                Just category ->
                                    Just
                                        { category
                                            | mods =
                                                category.mods
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

        OpenPanel categoryId id panel ->
            ( { model
                | mods =
                    Dict.update categoryId
                        (\v ->
                            case v of
                                Just category ->
                                    Just
                                        { category
                                            | mods =
                                                category.mods
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

        ToggleNewLinkForm categoryId ->
            ( { model
                | mods =
                    Dict.update categoryId
                        (\v ->
                            case v of
                                Just category ->
                                    Just
                                        { category
                                            | formIsHidden = not category.formIsHidden
                                            , isEditingCategoryTitle = False
                                            , newTitle_ = category.title
                                        }

                                Nothing ->
                                    Nothing
                        )
                        model.mods
              }
            , Cmd.none
            )

        SetNewUrl categoryId url ->
            -- todo debounce fetching suggested title
            ( { model
                | mods =
                    Dict.update categoryId
                        (\v ->
                            case v of
                                Just category ->
                                    Just
                                        { category
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
                            >> FetchTitleResponse categoryId
                        )
                    )
                |> Maybe.withDefault
                    (delay 0 (FetchTitleResponse categoryId (Success "")))
            )

        FetchTitleResponse categoryId (Success title) ->
            ( { model
                | mods =
                    Dict.update categoryId
                        (\v ->
                            case v of
                                Just category ->
                                    Just
                                        { category
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

        FetchTitleResponse categoryId (Failure why) ->
            ( model, Cmd.none )

        FetchTitleResponse categoryId _ ->
            ( model, Cmd.none )

        UseSuggestedTitle categoryId title ->
            update (SetNewTitle categoryId title)
                { model
                    | mods =
                        Dict.update categoryId
                            (\v ->
                                case v of
                                    Just category ->
                                        Just { category | suggestedTitle = NotAsked }

                                    Nothing ->
                                        Nothing
                            )
                            model.mods
                }

        SetNewTitle categoryId title ->
            ( { model
                | mods =
                    Dict.update categoryId
                        (\v ->
                            case v of
                                Just category ->
                                    Just { category | newTitle = title }

                                Nothing ->
                                    Nothing
                        )
                        model.mods
              }
            , Cmd.none
            )

        SetNewDescription categoryId description ->
            ( { model
                | mods =
                    Dict.update categoryId
                        (\v ->
                            case v of
                                Just category ->
                                    Just { category | newDescription = description }

                                Nothing ->
                                    Nothing
                        )
                        model.mods
              }
            , Cmd.none
            )

        AddLink categoryId ->
            --todo use id
            -- validate fields
            let
                updatedMods =
                    Dict.update categoryId
                        (\v ->
                            case v of
                                Just category ->
                                    Just { category | savingState = Loading }

                                Nothing ->
                                    Nothing
                        )
                        model.mods
            in
            ( { model
                | mods =
                    updatedMods
              }
            , updatedMods
                |> Dict.get categoryId
                |> Maybe.map
                    (\cat ->
                        let
                            newLink =
                                { url = cat.newUrl
                                , description = cat.newDescription
                                , title = cat.newTitle
                                }
                        in
                        Http.post
                            { url = "/api/"
                            , body =
                                Http.jsonBody <|
                                    Encode.object
                                        [ ( "tag", Encode.string "AddLink" )
                                        , ( "payload", encodeNewLink newLink )
                                        ]
                            , expect = Http.expectJson (RemoteData.fromResult >> AddLinkResponse categoryId) decodeMod
                            }
                    )
                |> Maybe.withDefault Cmd.none
            )

        AddLinkResponse categoryId (Success newLink) ->
            ( { model
                | mods =
                    Dict.update categoryId
                        (\v ->
                            case v of
                                Just category ->
                                    Just
                                        { category
                                            | mods = newLink :: category.mods
                                            , formIsHidden = True

                                            -- todo reset new link form fields
                                        }

                                Nothing ->
                                    Nothing
                        )
                        model.mods
              }
            , Cmd.none
            )

        AddLinkResponse categoryId _ ->
            -- todo error
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



--todo cateogry update helper
-- updateCategory categoryId (fn)
--todo new link update helper
-- updateNewLink cateogryId
-- updateCategory


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


delay n msg =
    Process.sleep n
        |> Task.perform (always msg)

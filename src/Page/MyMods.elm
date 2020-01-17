module Page.MyMods exposing (..)

import Data.Category as Category exposing (Category, CategoryId)
import Data.Link as Link exposing (Link, MorePanel(..))
import Data.Session exposing (Session)
import Data.User as User exposing (User)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Network.Api as Api
import Network.Scraper as Scraper
import Page.Profile as Profile
import Process
import RemoteData exposing (RemoteData(..), WebData)
import Task
import Url



-- roles
-- driver
-- business
-- admin
-- public
-- @TODO authentication
-- setup auth0
-- default to influencer role
-- DB TRIGGER add categories on signup
-- @TODO SOFT delete links
-- @todo SOFT delete categories
-- todo new link update helper
-- updateNewLink cateogryId
---- updateCategory
-- @TODO create and store default Modcategories Dict and union
-- db trigger?
-- @TODO profile page template renders model
-- store instagram id on User
-- update user schema and relations and triggers


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


type alias Mods =
    Dict CategoryId Category


initialModel session =
    { session = session
    , mods = Dict.fromList []
    , isNewCategoryFormVisible = False
    , newCategoryName = ""
    }


view : Model -> Html Msg
view model =
    let
        mods =
            model.mods
                |> Dict.toList
                |> List.map Tuple.second
                |> List.sortBy .order
    in
    Html.div []
        [ Html.div [ Attributes.class "md:px-8 mb-2" ]
            [ Html.p []
                [ Html.text "My car:"
                , Html.span [ Attributes.class "ml-1" ]
                    [ Html.text "2018 Subaru WRX Premium" ]
                ]
            ]
        , Html.div [ Attributes.class "flex md:flex-row flex-col" ]
            [ Html.div [ Attributes.class "flex-1" ]
                [ Html.div [ Attributes.class "mt-4 md:px-8 pb-8" ]
                    [ Html.div []
                        [ Html.p [ Attributes.class "capitalize text-center" ]
                            [ Html.text "my mods" ]
                        ]
                    , Html.ul
                        []
                        (mods
                            |> List.map
                                (\category ->
                                    Html.li [ Attributes.class "mb-4" ]
                                        [ Html.div [ Attributes.class "flex items-center w-full px-1 py-1 " ]
                                            [ Html.p
                                                [ Attributes.class "font-semibold mr-auto"
                                                , Attributes.classList [ ( "hidden", category.isEditingCategoryTitle ) ]
                                                ]
                                                [ Html.text category.name
                                                , Html.button
                                                    [ Attributes.class "text-xs ml-2"
                                                    , Events.onClick <| ToggleEditCategory category.id
                                                    ]
                                                    [ Html.text "Edit" ]
                                                ]
                                            , Html.form
                                                [ Attributes.class " mr-auto"
                                                , Attributes.classList
                                                    [ ( "hidden", not category.isEditingCategoryTitle )
                                                    , ( "font-semibold block", True )
                                                    ]
                                                , Events.onSubmit <| UpdateCategoryName category.id
                                                ]
                                                [ Html.input
                                                    [ Attributes.value category.newName
                                                    , Attributes.class "border px-1"
                                                    , Events.onInput <| SetNewCategoryTitle category.id
                                                    ]
                                                    []
                                                , Html.button
                                                    [ Attributes.class "ml-1 text-xs"
                                                    ]
                                                    [ Html.text "Save" ]
                                                ]
                                            , Html.button
                                                [ Attributes.type_ "button"
                                                , Attributes.class "py-2 px-1 text-sm "
                                                , Events.onClick <| ToggleNewLinkForm category.id
                                                ]
                                                [ Html.text "Add Link"
                                                , Html.p
                                                    [ Attributes.class "text-right  text-xs text-gray-500 pb-px"
                                                    ]
                                                    [ Html.text <|
                                                        String.join " "
                                                            [ List.length category.links
                                                                |> String.fromInt
                                                            , "links"
                                                            ]
                                                    ]
                                                ]
                                            ]

                                        -- ,
                                        , viewNewLinkForm category
                                        , Html.div [ Attributes.class "px-px py-px bg-gray-200 rounded-sm" ]
                                            (if True then
                                                category.links
                                                    |> List.map (viewLink category.id)

                                             else
                                                category.links
                                                    |> List.take 3
                                                    |> List.map (viewLink category.id)
                                            )
                                        ]
                                )
                        )
                    , Html.button
                        [ Attributes.class "px-4 py-1 font-medium text-center rounded-sm border my-3 "
                        , Events.onClick ToggleNewCategoryForm
                        ]
                        [ Html.text "+ Add New Category" ]
                    , viewIf model.isNewCategoryFormVisible <|
                        Html.form
                            [ Events.onSubmit AddNewCategory
                            ]
                            [ Html.label [ Attributes.class "font-medium" ]
                                [ Html.text "Name:"
                                , Html.input
                                    [ Attributes.class "ml-1 px-2 border py-1 mx-1"
                                    , Attributes.placeholder "Audio/Video"
                                    , Events.onInput SetNewCategoryName
                                    , Attributes.value model.newCategoryName
                                    ]
                                    []
                                ]
                            , Html.button
                                [ Attributes.class "px-4 py-1 font-medium text-center rounded-sm border"
                                ]
                                [ Html.text "Save" ]
                            ]
                    ]
                ]
            , Html.div [ Attributes.class "flex-1" ]
                [ Profile.view (List.filter (.links >> List.isEmpty >> not) mods) ]
            ]
        ]


viewNewLinkForm : Category -> Html Msg
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
                    , Attributes.value category.newTitle
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
        [ Html.div [ Attributes.class "bg-white my-px px-2 py-1 rounded-sm" ]
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
                        , Attributes.checked link.isActive
                        , Attributes.class "ml-1"
                        , Events.onClick <| ToggleLinkActive categoryId link
                        ]
                        []
                    ]
                , Html.div [ Attributes.class "ml-auto" ]
                    [ Html.button
                        [ Attributes.class "mx-1"
                        , Events.onClick <| DeleteLink categoryId link
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


type Msg
    = InitializeMyMods
    | Initialized (Result Api.Error Mods)
    | ToggleNewLinkForm CategoryId
    | SetNewUrl CategoryId String
    | FetchTitleResponse CategoryId (WebData String)
    | UseSuggestedTitle CategoryId String
    | SetNewTitle CategoryId String
    | SetNewDescription CategoryId String
    | AddLink CategoryId
    | AddLinkResponse CategoryId (Result Api.Error Link)
    | OpenPanel CategoryId Link.Id MorePanel
    | ClosePanel CategoryId Link.Id
    | ToggleEditCategory CategoryId
    | SetNewCategoryTitle CategoryId String
    | UpdateCategoryName CategoryId
    | ToggleNewCategoryForm
    | AddNewCategory
    | CategoryResponse (Result Api.Error Category)
    | SetNewCategoryName String
    | DeleteLink CategoryId Link
    | DeleteLinkResponse CategoryId (Result Api.Error Link.Id)
    | ToggleLinkActive CategoryId Link
    | ToggleLinkActiveResponse CategoryId (Result Api.Error Link)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        session =
            model.session
    in
    case msg of
        InitializeMyMods ->
            let
                document =
                    Api.document Category.queryDocument []
            in
            ( model
            , Api.query session (User.sessionToken session) document Nothing Category.decodeModCategories
                |> Task.attempt Initialized
            )

        Initialized (Ok mods) ->
            ( { model | mods = Dict.union mods model.mods }, Cmd.none )

        Initialized _ ->
            ( model, Cmd.none )

        SetNewCategoryName name ->
            ( { model | newCategoryName = name }, Cmd.none )

        AddNewCategory ->
            case session.user of
                User.Driver token profile ->
                    let
                        order =
                            1 + List.length (Dict.toList model.mods)

                        encodedVars =
                            -- todo move order and owner to backend
                            Encode.object
                                [ ( "objects"
                                  , Encode.list Category.encode
                                        [ { name = model.newCategoryName
                                          , order = order
                                          , owner = User.idToString profile.id
                                          }
                                        ]
                                  )
                                ]
                                |> Just

                        decoder =
                            Decode.succeed identity
                                |> Decode.requiredAt [ "insert_categories", "returning", "0" ]
                                    Category.decodeCategory
                    in
                    ( model
                    , Api.query session
                        token
                        (Api.document Category.insert [])
                        encodedVars
                        decoder
                        |> Task.attempt CategoryResponse
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateCategoryName categoryId ->
            case Dict.get categoryId model.mods of
                Just { newName } ->
                    let
                        encodedVars =
                            Encode.object
                                [ ( "id", Encode.string categoryId )
                                , ( "name", Encode.string newName )
                                ]

                        decoder =
                            Decode.succeed identity
                                |> Decode.requiredAt [ "update_categories", "returning" ]
                                    (Decode.index 0 Category.decodeCategory)
                    in
                    ( model
                    , Api.query session (User.sessionToken session) (Api.document Category.update []) (Just encodedVars) decoder
                        |> Task.attempt CategoryResponse
                    )

                Nothing ->
                    ( model, Cmd.none )

        CategoryResponse (Ok category) ->
            ( { model
                | mods = Dict.insert category.id category model.mods
                , isNewCategoryFormVisible = False
                , newCategoryName = ""
              }
            , Cmd.none
            )

        CategoryResponse x ->
            ( model, Cmd.none )

        ToggleNewCategoryForm ->
            ( { model | isNewCategoryFormVisible = not model.isNewCategoryFormVisible }, Cmd.none )

        SetNewCategoryTitle categoryId title ->
            ( { model
                | mods =
                    updateCategory categoryId
                        (\category ->
                            { category
                                | formIsHidden = True
                                , newName = title
                            }
                        )
                        model.mods
              }
            , Cmd.none
            )

        ToggleEditCategory categoryId ->
            ( { model
                | mods =
                    updateCategory categoryId
                        (\category ->
                            { category
                                | isEditingCategoryTitle = not category.isEditingCategoryTitle
                                , formIsHidden = True
                            }
                        )
                        model.mods
              }
            , Cmd.none
            )

        ClosePanel categoryId id ->
            ( { model
                | mods =
                    updateCategory categoryId
                        (\category ->
                            { category
                                | links =
                                    category.links
                                        |> List.map
                                            (\link ->
                                                if link.id == id then
                                                    { link | panel = Nothing }

                                                else
                                                    link
                                            )
                            }
                        )
                        model.mods
              }
            , Cmd.none
            )

        OpenPanel categoryId id panel ->
            ( { model
                | mods =
                    updateCategory categoryId
                        (\category ->
                            { category
                                | links =
                                    category.links
                                        |> List.map
                                            (\m ->
                                                if m.id == id then
                                                    { m | panel = Just panel }

                                                else
                                                    m
                                            )
                            }
                        )
                        model.mods
              }
            , Cmd.none
            )

        ToggleNewLinkForm categoryId ->
            ( { model
                | mods =
                    updateCategory categoryId
                        (\category ->
                            { category
                                | formIsHidden = not category.formIsHidden
                                , isEditingCategoryTitle = False
                                , newName = category.name
                            }
                        )
                        model.mods
              }
            , Cmd.none
            )

        SetNewUrl categoryId url ->
            -- todo debounce fetching suggested title
            ( { model
                | mods =
                    updateCategory categoryId
                        (\category ->
                            { category
                                | newUrl = url
                                , suggestedTitle = Loading
                                , savingState = NotAsked
                            }
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
                    updateCategory categoryId
                        (\category ->
                            { category
                                | suggestedTitle = Success title
                                , newTitle = title
                            }
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
                        updateCategory categoryId
                            (\category ->
                                { category | suggestedTitle = NotAsked }
                            )
                            model.mods
                }

        SetNewTitle categoryId title ->
            ( { model
                | mods =
                    updateCategory categoryId
                        (\category ->
                            { category | newTitle = title }
                        )
                        model.mods
              }
            , Cmd.none
            )

        SetNewDescription categoryId description ->
            ( { model
                | mods =
                    updateCategory categoryId
                        (\category ->
                            { category | newDescription = description }
                        )
                        model.mods
              }
            , Cmd.none
            )

        AddLink categoryId ->
            -- @TODO validate fields before submitting
            --
            let
                updatedMods =
                    updateCategory categoryId
                        (\category ->
                            { category | savingState = Loading }
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
                        case Url.fromString cat.newUrl of
                            Just urlParts ->
                                let
                                    newLink =
                                        { urlString = cat.newUrl
                                        , description = cat.newDescription
                                        , title = cat.newTitle
                                        , category_id = categoryId
                                        , fragment = urlParts.fragment
                                        , host = urlParts.host
                                        , path = urlParts.path
                                        , protocol = urlParts.protocol
                                        , query = urlParts.query
                                        }

                                    encodedVars =
                                        Encode.object
                                            [ ( "objects"
                                              , Encode.list Link.encode [ newLink ]
                                              )
                                            ]

                                    decoder =
                                        Decode.succeed identity
                                            |> Decode.requiredAt [ "insert_links", "returning" ] (Decode.index 0 Link.decode)
                                in
                                Api.query session (User.sessionToken session) (Api.document Link.insert []) (Just encodedVars) decoder
                                    |> Task.attempt (AddLinkResponse categoryId)

                            Nothing ->
                                Cmd.none
                    )
                |> Maybe.withDefault Cmd.none
            )

        AddLinkResponse categoryId (Ok newLink) ->
            ( { model
                | mods =
                    updateCategory categoryId
                        (\category ->
                            { category
                                | links = newLink :: category.links
                                , formIsHidden = True

                                -- todo reset new link form fields after refactor to model
                            }
                        )
                        model.mods
              }
            , Cmd.none
            )

        AddLinkResponse categoryId _ ->
            -- todo error handling
            ( model, Cmd.none )

        DeleteLink categoryId link ->
            let
                encodedVars =
                    Encode.object [ ( "id", Link.encodeId link.id ) ]
                        |> Just

                decoder =
                    Decode.succeed identity
                        |> Decode.requiredAt [ "update_links", "returning" ]
                            (Decode.index 0 (Decode.value |> Decode.map (always link.id)))
            in
            ( model
            , Api.query session (User.sessionToken session) (Api.document Link.deleteLink []) encodedVars decoder
                |> Task.attempt (DeleteLinkResponse categoryId)
            )

        DeleteLinkResponse categoryId (Ok id) ->
            ( { model
                | mods =
                    updateCategory categoryId
                        (\category ->
                            { category | links = List.filter (.id >> (/=) id) category.links }
                        )
                        model.mods
              }
            , Cmd.none
            )

        DeleteLinkResponse categoryId _ ->
            ( model, Cmd.none )

        ToggleLinkActive categoryId link ->
            let
                encodedVars =
                    Encode.object
                        [ ( "id", Link.encodeId link.id )
                        , ( "is_active", Encode.bool <| not link.isActive )
                        ]
                        |> Just

                decoder =
                    Decode.succeed identity
                        |> Decode.requiredAt [ "update_links", "returning" ]
                            (Decode.index 0 Link.decode)
            in
            ( model
            , Api.query session (User.sessionToken session) (Api.document Link.updateIsActive []) encodedVars decoder
                |> Task.attempt (ToggleLinkActiveResponse categoryId)
            )

        ToggleLinkActiveResponse categoryId (Ok updatedLink) ->
            ( { model
                | mods =
                    updateCategory categoryId
                        (\category ->
                            { category
                                | links =
                                    category.links
                                        |> List.map
                                            (\link ->
                                                if link.id == updatedLink.id then
                                                    updatedLink

                                                else
                                                    link
                                            )
                            }
                        )
                        model.mods
              }
            , Cmd.none
            )

        ToggleLinkActiveResponse categoryId link ->
            -- todo error handling
            ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateCategory categoryId fn =
    Dict.update categoryId (Maybe.map fn)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


delay n msg =
    Process.sleep n
        |> Task.perform (always msg)


viewIf bool html =
    if bool then
        html

    else
        Html.text ""

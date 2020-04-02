module Page.MyMods exposing (..)

import Data.Category as Category exposing (Category, CategoryId)
import Data.Link as Link exposing (Link, MorePanel(..))
import Data.Session as Session exposing (Session)
import Data.User as User exposing (DriverProfile, User)
import Data.Vehicle as Vehicle
import Dict exposing (Dict)
import File
import File.Select
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Extra as Html
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Network.Api as Api
import Network.Scraper as Scraper
import Network.SignedUrl as SignedUrl
import Network.User as User
import Network.Util as Util
import Network.Vehicle as Vehicle
import Page.Profile as Profile
import Process
import RemoteData exposing (RemoteData(..), WebData)
import Task
import Time exposing (Posix)
import Url



-- roles
--- driver
--- business
--- admin
--- public
-- @todo SOFT delete categories
-- todo new link update helper
-- updateNewLink cateogryId
---- updateCategory
-- todo profile stuff
-- update bio
-- update car make and model
-- update avatar
-- start analytics
-- research linktrees analytics offerings
-- todo custom element for public profiles's avatar
--- on error load placeholder?
-- consider adding updated_at to bust cache
-- @todo cascading deletes
-- double check permissions
-- hosting


type alias Model =
    { accessToken : User.AccessToken
    , isNewCategoryFormVisible : Bool -- refactor to maybe?
    , mods : Mods
    , newCategoryName : String
    , driverProfile : User.DriverProfile
    , profileForm : Form
    , session : Session
    }


initialModel : Session -> User.AccessToken -> User.DriverProfile -> Model
initialModel session accessToken driverProfile =
    { accessToken = accessToken
    , isNewCategoryFormVisible = False
    , mods = Dict.fromList []
    , newCategoryName = "" -- @todo form?
    , driverProfile = driverProfile
    , profileForm =
        case driverProfile.profile of
            Just profile ->
                { bio = profile.bio
                , maybeAvatarFile = Nothing
                , maybeNewAvatar = Nothing
                , maybeNewUsername = driverProfile.username
                , vehicleMake = profile.vehicleMake
                , vehicleModel = profile.vehicleModel
                , vehicleYear = profile.vehicleYear
                }

            Nothing ->
                { bio = ""
                , maybeAvatarFile = Nothing
                , maybeNewAvatar = Nothing
                , maybeNewUsername = driverProfile.username
                , vehicleMake = ""
                , vehicleModel = ""
                , vehicleYear = ""
                }
    , session = session
    }


type alias Form =
    { bio : String
    , maybeAvatarFile : Maybe File.File
    , maybeNewAvatar : Maybe String
    , maybeNewUsername : Maybe String
    , vehicleMake : String
    , vehicleModel : String
    , vehicleYear : String
    }


type alias NewLink =
    { url : String
    , description : String
    , title : String
    }


type alias Mods =
    Dict CategoryId Category


view : Model -> Html Msg
view model =
    let
        session =
            model.session

        mods =
            model.mods
                |> Dict.toList
                |> List.map Tuple.second
                |> List.sortBy .order
    in
    Html.div []
        [ Html.div [ Attributes.class "flex md:flex-row flex-col" ]
            [ Html.div [ Attributes.class "flex-1 md:w-1/2" ]
                [ Html.div [ Attributes.class "mt-4 md:px-8 pb-8" ]
                    [ viewMyBio model
                    , Html.div []
                        [ Html.p [ Attributes.class "capitalize text-center font-semibold" ]
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
                                                [ Attributes.class "font-medium mr-auto"
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
                                                    , ( "font-medium block", True )
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
            , Html.div [ Attributes.class "flex-1 hidden sm:block pt-12" ]
                [ Html.div
                    [ Attributes.class "border-2 border-black mx-auto rounded-sm overflow-scroll"
                    , Attributes.style "width" "320px"
                    , Attributes.style "height" "529px"
                    ]
                    [ mods
                        |> List.filter
                            (.links >> List.isEmpty >> not)
                        |> Profile.view model.driverProfile
                        |> Html.map ProfileMsg
                    ]
                ]
            ]
        ]


viewMyBio : Model -> Html Msg
viewMyBio { profileForm, driverProfile } =
    Html.div [ Attributes.class "w-full md:w-1/x2 md:mb-8" ]
        [ Html.p [ Attributes.class "font-semibold text-center" ]
            [ Html.text "My car:"
            ]
        , Html.div [ Attributes.class "mb-2 flex items-center" ]
            [ Html.div []
                [ Html.label [ Attributes.class "block font-medium mb-1" ]
                    [ Html.text "Avatar"
                    ]
                , Html.button
                    [ Attributes.class "ml-2 px-4 py-2 font-medium text-center rounded-sm border block w-32 mb-1"
                    , Attributes.classList [ ( "hidden", profileForm.maybeNewAvatar /= Nothing ) ]
                    , Events.onClick AskForAvatarFile
                    ]
                    [ Html.text "Select image" ]
                , Html.viewIf (profileForm.maybeNewAvatar /= Nothing) <|
                    Html.div [ Attributes.class "" ]
                        [ Html.button
                            [ Attributes.class "border-green-700 text-white bg-green-500 ml-2 px-4 py-2 font-medium text-center rounded-sm border block w-32 mb-1"
                            , Events.onClick SaveNewAvatar
                            ]
                            [ Html.text "Save" ]
                        , Html.button
                            [ Attributes.class "border-red-700 text-red-500 ml-2 px-4 py-2 font-medium text-center rounded-sm border block w-32 mb-1"
                            , Events.onClick RemoveNewAvatar
                            ]
                            [ Html.text "Cancel" ]
                        ]
                ]
            , Html.img
                [ Attributes.class "w-32 mx-auto"
                , case profileForm.maybeNewAvatar of
                    Just avatar ->
                        Attributes.src avatar

                    Nothing ->
                        Attributes.src <|
                            String.concat
                                [ "https://dev-mods-in-bio.s3.amazonaws.com/"
                                , User.idToString driverProfile.id
                                , "?last_updated="
                                , String.fromInt <| Time.posixToMillis driverProfile.lastUpdated
                                ]
                ]
                []
            ]
        , Html.form [ Events.onSubmit SaveMyProfile ]
            [ Html.div [ Attributes.class "mb-2" ]
                [ Html.label [ Attributes.class "mr-1 block font-medium mb-1" ]
                    [ Html.text "Username" ]
                , Html.input
                    [ Attributes.class "border rounded-sm block w-full px-2 py-1"
                    , Attributes.placeholder "@yourinsta"
                    , Events.onInput SetUsername
                    , profileForm.maybeNewUsername
                        |> Maybe.map Attributes.value
                        |> Maybe.withDefault (Attributes.value "")
                    ]
                    []
                ]
            , Html.div [ Attributes.class "mb-2" ]
                [ Html.label [ Attributes.class "mr-1 block font-medium mb-1" ]
                    [ Html.text "Year" ]
                , Html.input
                    [ Attributes.class "border rounded-sm block w-full px-2 py-1"
                    , Attributes.type_ "text"
                    , Attributes.pattern "[0-9]*"
                    , Attributes.attribute "inputmode" "numeric"
                    , Attributes.placeholder "2018"
                    , Events.onInput SetVehicleYear
                    , Attributes.value profileForm.vehicleYear
                    ]
                    []
                ]
            , Html.div [ Attributes.class "mb-2" ]
                [ Html.label [ Attributes.class "mb-1 mr-1 block font-medium" ] [ Html.text "Make" ]
                , Html.input
                    [ Attributes.class "border rounded-sm block w-full px-2 py-1"
                    , Attributes.id "bio-car-make"
                    , Attributes.list "bio-car-make--options"
                    , Attributes.placeholder "Subaru"
                    , Events.onInput VehicleMakeSelected
                    , Attributes.value profileForm.vehicleMake
                    ]
                    []
                ]
            , Html.div [ Attributes.class "mb-2" ]
                [ Html.label [ Attributes.class "mb-1  block font-medium" ] [ Html.text "Model" ]
                , Html.input
                    [ Attributes.class "border rounded-sm block w-full px-2 py-1"
                    , Attributes.id "bio-car-model"
                    , Attributes.list "bio-car-model--options"
                    , Attributes.placeholder "WRX"
                    , Events.onInput SetVehicleModel
                    , Attributes.value profileForm.vehicleModel
                    ]
                    []
                ]
            , Html.div [ Attributes.class "mb-2" ]
                [ Html.label [ Attributes.class "mb-1 block font-medium" ] [ Html.text "Bio" ]
                , Html.textarea
                    [ Attributes.class "block w-full border rounded-sm px-2 py-1"
                    , Attributes.placeholder "Premium trim"
                    , Attributes.value profileForm.bio
                    , Events.onInput SetBio
                    ]
                    []
                ]
            , Html.div [ Attributes.class "" ]
                [ Html.button
                    [ Attributes.class "px-4 py-2 font-medium text-center rounded-sm border"
                    , Attributes.type_ "submit"
                    ]
                    [ Html.text "Save" ]
                ]
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
    = Initialized (Result Api.Error Mods)
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
    | ProfileMsg Profile.Msg
    | VehicleMakeSelected String
    | SetVehicleYear String
    | SetVehicleModel String
    | SetBio String
    | SaveMyProfile
    | SaveMyProfileResponse Form (Result Api.Error ())
    | AskForAvatarFile
    | AvatarImageLoaded File.File
    | AvatarImageBase64 (Result String String)
    | RemoveNewAvatar
    | SaveNewAvatar
    | SaveNewAvatarResponse (Result Http.Error Posix)
    | SetUsername String
    | NoOp


init : Session -> User.AccessToken -> User.DriverProfile -> ( Model, Cmd Msg )
init session accessToken driverProfile =
    let
        document =
            Api.document Category.queryDocument []
    in
    ( initialModel session accessToken driverProfile
    , Api.authedQuery accessToken document Nothing Category.decodeModCategories
        |> Task.attempt Initialized
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        session =
            model.session

        accessToken =
            model.accessToken
    in
    case msg of
        Initialized (Ok mods) ->
            ( { model | mods = Dict.union mods model.mods }, Cmd.none )

        Initialized _ ->
            ( model, Cmd.none )

        SetNewCategoryName name ->
            ( { model | newCategoryName = name }, Cmd.none )

        AddNewCategory ->
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
                                  , owner = User.idToString model.driverProfile.id
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
            , Api.authedQuery
                accessToken
                (Api.document Category.insert [])
                encodedVars
                decoder
                |> Task.attempt CategoryResponse
            )

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
                    , Api.authedQuery accessToken (Api.document Category.update []) (Just encodedVars) decoder
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
                                              , Encode.list Link.encoder [ newLink ]
                                              )
                                            ]

                                    decoder =
                                        Decode.succeed identity
                                            |> Decode.requiredAt [ "insert_links", "returning" ] (Decode.index 0 Link.decoder)
                                in
                                Api.authedQuery accessToken (Api.document Link.insert []) (Just encodedVars) decoder
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
                                , newDescription = ""
                                , newTitle = ""
                                , newName = ""
                                , suggestedTitle = NotAsked
                                , newUrl = ""
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
            , Api.authedQuery accessToken (Api.document Link.deleteLink []) encodedVars decoder
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
                            (Decode.index 0 Link.decoder)
            in
            ( model
            , Api.authedQuery accessToken (Api.document Link.updateIsActive []) encodedVars decoder
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

        ProfileMsg subMsg ->
            ( model, Cmd.none )

        VehicleMakeSelected make ->
            let
                profileForm =
                    model.profileForm

                p =
                    { profileForm | vehicleMake = make }
            in
            ( { model | profileForm = p }, Cmd.none )

        SetVehicleYear year ->
            let
                profileForm =
                    model.profileForm

                p =
                    { profileForm | vehicleYear = year }
            in
            ( { model
                | profileForm = p
              }
            , Cmd.none
            )

        SetVehicleModel vehicleModel ->
            let
                profileForm =
                    model.profileForm

                p =
                    { profileForm | vehicleModel = vehicleModel }
            in
            ( { model | profileForm = p }, Cmd.none )

        SetBio bio ->
            let
                profileForm =
                    model.profileForm

                p =
                    { profileForm | bio = bio }
            in
            ( { model | profileForm = p }, Cmd.none )

        SaveMyProfile ->
            let
                profileForm =
                    model.profileForm
            in
            ( model
            , User.updateUserMutation accessToken model.driverProfile.id profileForm model.profileForm.maybeNewUsername
                |> Task.attempt (SaveMyProfileResponse profileForm)
            )

        SaveMyProfileResponse { bio, vehicleMake, vehicleModel, vehicleYear, maybeNewUsername } (Ok ()) ->
            let
                profile =
                    { bio = bio
                    , vehicleMake = vehicleMake
                    , vehicleModel = vehicleModel
                    , vehicleYear = vehicleYear
                    }

                driverProfile =
                    model.driverProfile

                driverProfile_ =
                    { driverProfile
                        | profile = Just profile
                        , username = maybeNewUsername
                    }
            in
            ( { model | driverProfile = driverProfile_ }
            , Session.saveUser accessToken driverProfile_
            )

        SaveMyProfileResponse _ _ ->
            ( model, Cmd.none )

        AskForAvatarFile ->
            ( model, File.Select.file [ "image/*" ] AvatarImageLoaded )

        AvatarImageLoaded file ->
            let
                profileForm =
                    model.profileForm

                p =
                    { profileForm | maybeAvatarFile = Just file }
            in
            ( { model | profileForm = p }
            , File.toUrl file
                |> Task.attempt AvatarImageBase64
            )

        AvatarImageBase64 (Ok str) ->
            updateForm (\profileForm -> { profileForm | maybeNewAvatar = Just str }) model

        AvatarImageBase64 (Err str) ->
            -- @todo
            ( model, Cmd.none )

        RemoveNewAvatar ->
            updateForm
                (\profileForm ->
                    { profileForm
                        | maybeNewAvatar = Nothing
                        , maybeAvatarFile = Nothing
                    }
                )
                model

        SaveNewAvatar ->
            case model.profileForm.maybeAvatarFile of
                Just file ->
                    let
                        mimeType =
                            File.mime file
                    in
                    ( model
                    , SignedUrl.getSignedUrl accessToken model.driverProfile.id mimeType
                        |> Task.andThen
                            (\url ->
                                uploadFile accessToken { url = url, file = file }
                            )
                        |> Task.andThen
                            (\_ ->
                                Time.now
                            )
                        |> Task.attempt SaveNewAvatarResponse
                    )

                Nothing ->
                    ( model, Cmd.none )

        SaveNewAvatarResponse (Ok now) ->
            let
                driverProfile =
                    model.driverProfile

                driverProfile_ =
                    { driverProfile | lastUpdated = now }
            in
            update RemoveNewAvatar { model | driverProfile = driverProfile_ }

        SaveNewAvatarResponse (Err _) ->
            -- @todo
            ( model, Cmd.none )

        SetUsername s ->
            let
                profileForm =
                    model.profileForm

                p =
                    { profileForm | maybeNewUsername = Just s }

                p_ =
                    { profileForm | maybeNewUsername = Nothing }
            in
            if String.trim s /= "" then
                ( { model | profileForm = p }, Cmd.none )

            else
                ( { model | profileForm = p_ }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateCategory categoryId fn =
    Dict.update categoryId (Maybe.map fn)


updateForm fn model =
    ( { model | profileForm = fn model.profileForm }, Cmd.none )


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


uploadFile token { url, file } =
    -- todo tracking
    Http.task
        { method = "put"
        , headers = [] --  (Api.authHeaders token)
        , url = url
        , body = Http.fileBody file
        , resolver = Http.stringResolver Util.resolver
        , timeout = Nothing
        }

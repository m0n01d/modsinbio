module Page.Profile exposing (Model, Msg(..), init, page, update, view)

import Data.Category as Category exposing (Category)
import Data.Link as Link exposing (Link)
import Data.Session as Session
import Data.User as User exposing (DriverProfile)
import Dict
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Extra as Html
import Http
import Json.Decode as Decode
import Network.Api as Api
import Network.Link as Link
import Network.User as User
import Task


page : Model -> { title : String, content : Html Msg }
page model =
    case model.profile of
        Just { profile, mods } ->
            let
                mods_ =
                    Dict.toList mods
                        |> List.map Tuple.second
                        |> List.filter (.links >> List.isEmpty >> not)
                        |> List.sortBy .order
            in
            { title = profile.username
            , content = view mods_ profile
            }

        Nothing ->
            -- @TODO error handling
            { title = "err", content = Html.text "woop" }


view : List Category -> DriverProfile -> Html Msg
view mods profile =
    Html.div
        [ Attributes.class "max-h-full  overflow-y-scroll"
        ]
        [ Html.div
            [ Attributes.class "  rounded max-w-full max-w-full "
            ]
            [ Html.div [ Attributes.class "bg-white" ]
                [ Html.div []
                    [ Html.div
                        [ Attributes.class "bg-center bg-cover block max-w-full mx-auto h-32"
                        , Attributes.style "background-image" <|
                            String.concat
                                [ "url("
                                , String.concat
                                    [ "https://dev-mods-in-bio.s3.amazonaws.com/"
                                    , User.idToString profile.id
                                    ]
                                , ")"
                                ]
                        ]
                        []
                    , Html.div [ Attributes.class "px-2" ]
                        [ Html.h1 [ Attributes.class "text-left font-medium text-lg mt-2 mb-px" ]
                            [ Html.a
                                [ Attributes.href <| String.concat [ "https://instagram.com/", profile.username ]
                                , Attributes.target "_blank"
                                , Attributes.rel "noopener"
                                ]
                                [ Html.text <| String.concat [ "@", profile.username ]
                                ]
                            ]
                        , case profile.profile of
                            Just { bio, vehicleMake, vehicleYear, vehicleModel } ->
                                Html.div [ Attributes.class "px-" ]
                                    [ Html.p []
                                        [ Html.text <|
                                            -- @todo add trim
                                            String.join " " [ vehicleYear, vehicleMake, vehicleModel ]
                                        ]
                                    , Html.p [] [ Html.text bio ]
                                    ]

                            Nothing ->
                                Html.nothing
                        , Html.ul [ Attributes.class "" ]
                            (mods
                                |> List.map viewCategory
                            )
                        ]
                    ]
                ]
            ]
        ]


viewCategory : Category -> Html Msg
viewCategory { name, links } =
    let
        firstChunk =
            List.take 3 links

        theRest =
            List.drop 3 links
    in
    Html.div [ Attributes.class "my-4" ]
        [ Html.p [ Attributes.class "font-semibold text-sm px-px sticky top-0 bg-white py-2" ]
            [ Html.text name ]
        , Html.ul []
            (firstChunk |> List.map viewPreviewLink)
        , Html.viewIf (not (List.isEmpty theRest)) <|
            Html.node "ui-openable"
                []
                [ Html.button
                    [ Attributes.class "text-center block w-full text-gray-700"
                    , Attributes.attribute "Openable__activator" ""
                    ]
                    [ Html.text "+ View all" ]
                , Html.ul
                    [ Attributes.class "hidden"
                    , Attributes.attribute "Openable__content" ""
                    ]
                    (List.map viewPreviewLink theRest)
                ]
        ]


viewPreviewLink : Link -> Html Msg
viewPreviewLink { title, description, urlString, id } =
    Html.li []
        [ Html.div [ Attributes.class "my-3 px-1" ]
            [ Html.div []
                [ Html.node "ui-link-click"
                    [ Decode.succeed (LinkClicked id)
                        |> Events.on "LinkClicked"
                    ]
                    [ Html.a
                        [ Attributes.class "group text-sm md:text-base leading-tight text-center px-2 py-3 border border-green-600 mt-2 block rounded-sm bg-green-500 text-white hover:bg-white hover:text-green-500"
                        , Attributes.href urlString
                        , Attributes.target "_blnk"
                        , Attributes.rel "noopener"
                        ]
                        [ Html.text title ]
                    ]
                ]
            , Html.viewIf (not (String.isEmpty description)) <|
                Html.node "ui-openable"
                    []
                    [ Html.button
                        [ Attributes.class "block bg-gray-300 text-gray-800 w-full text-lg font-bold monospace mt-px"
                        , Attributes.attribute "Openable__activator" ""
                        ]
                        [ Html.text "···"
                        ]
                    , Html.div
                        [ Attributes.class "border mt-0 px-1 py-2 text-sm text-gray-900"
                        , Attributes.classList [ ( "hidden", True ) ]
                        , Attributes.attribute "Openable__content" ""
                        ]
                        [ Html.p [] [ Html.text description ]
                        ]
                    , Html.button
                        [ Attributes.class "hidden block bg-gray-300 text-gray-800 w-full text-lg font-bold monospace mt-px"
                        , Attributes.attribute
                            "Openable__deactivator"
                            ""
                        ]
                        [ Html.text "↑" ]
                    ]
            ]
        ]


type Msg
    = NoOp
    | GotProfile (Result Api.Error User.PublicProfile)
    | LinkClicked Link.Id
    | LinkClickedResponse (Result Api.Error ())
    | IncrementedView (Result Http.Error ())


type alias Model =
    { session : Session.Session
    , profile : Maybe User.PublicProfile
    }


init : Session.Session -> String -> ( Model, Cmd Msg )
init session username =
    ( { session = session, profile = Nothing }
    , User.profileQuery username
        |> Task.attempt GotProfile
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotProfile (Ok profile) ->
            ( { model | profile = Just profile }
            , User.incrementViewCount profile.profile IncrementedView
            )

        GotProfile (Err err) ->
            ( model, Cmd.none )

        LinkClicked id ->
            ( model
            , Link.clickedMutation id
                |> Task.attempt LinkClickedResponse
            )

        LinkClickedResponse res ->
            -- @todo error handling
            ( model, Cmd.none )

        IncrementedView res ->
            -- @todo error handling
            ( model, Cmd.none )

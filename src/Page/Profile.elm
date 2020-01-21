module Page.Profile exposing (Model, Msg(..), init, page, update, view)

import Data.Category as Category exposing (Category)
import Data.Link exposing (Link)
import Data.Session as Session
import Data.User as User exposing (DriverProfile)
import Dict
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Extra as Html
import Network.Api as Api
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
                        [ Html.text <| String.concat [ "@", profile.username ] ]
                    , Html.p [] [ Html.text "2018 wrx premium" ]
                    , Html.p [] [ Html.text "bio?" ]
                    , Html.ul []
                        (mods
                            |> List.map viewCategory
                        )
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
        [ Html.p [ Attributes.class "font-semibold text-sm" ]
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



-- custom element opanable


viewPreviewLink : Link -> Html Msg
viewPreviewLink { title, description } =
    Html.li []
        [ Html.div [ Attributes.class "my-3 px-1" ]
            [ Html.div []
                [ Html.a
                    [ Attributes.class "group text-sm md:text-base leading-tight text-center px-2 py-3 border border-green-600 mt-2 block rounded-sm bg-green-500 text-white hover:bg-white hover:text-green-500"
                    , Attributes.href "https://www.fastwrx.com/collections/shift-knobs/products/cobb-6-speed-shift-knob"
                    ]
                    [ Html.text title ]
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


type alias Model =
    { session : Session.Session
    , profile : Maybe User.PublicProfile
    }


init : Session.Session -> String -> ( Model, Cmd Msg )
init session username =
    -- query for user
    ( { session = session, profile = Nothing }
    , User.profileDocument username
        |> Task.attempt GotProfile
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotProfile (Ok profile) ->
            ( { model | profile = Just profile }, Cmd.none )

        GotProfile (Err err) ->
            let
                _ =
                    Debug.log "e" err
            in
            ( model, Cmd.none )

module Page.Home exposing (..)

import Data.Session exposing (Session)
import Data.User as User
import Dict
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Network.Api as Api
import Network.SignedUrl as SignedUrl
import Network.User as User
import Network.Util as Util
import Page.Profile as Profile
import Route
import Task
import Url.Builder


type alias Model =
    { session : Session
    , profile : Maybe User.PublicProfile
    , maybeEmail : Maybe String
    , savedEmail : Bool
    }


type Msg
    = NoOp
    | ProfileMsg Profile.Msg
    | GotProfile (Result Api.Error User.PublicProfile)
    | SetEmail String
    | SaveEmail
    | SaveEmailResponse (Result Http.Error String)


initialModel session =
    { session = session, profile = Nothing, maybeEmail = Nothing, savedEmail = False }


init : Session -> String -> ( Model, Cmd Msg )
init session username =
    ( initialModel session
    , User.profileQuery username
        |> Task.attempt GotProfile
    )


update msg model =
    case msg of
        GotProfile (Ok profile) ->
            ( { model | profile = Just profile }
            , Cmd.none
            )

        GotProfile (Err err) ->
            ( model, Cmd.none )

        ProfileMsg _ ->
            ( model, Cmd.none )

        SetEmail email ->
            ( { model | maybeEmail = Just email }, Cmd.none )

        SaveEmail ->
            case model.maybeEmail of
                Just email ->
                    ( model
                    , saveEmail email
                        |> Task.attempt SaveEmailResponse
                    )

                Nothing ->
                    ( model, Cmd.none )

        SaveEmailResponse (Ok res) ->
            ( { model | maybeEmail = Just res, savedEmail = True }, Cmd.none )

        _ ->
            ( model, Cmd.none )


saveEmail email =
    Http.task
        { method = "get"
        , headers = []
        , body = Http.emptyBody
        , resolver = Http.stringResolver Util.resolver
        , url =
            Url.Builder.absolute [ "api", "thanks" ]
                [ Url.Builder.string "key" email
                ]
        , timeout = Nothing
        }


view : Model -> Html Msg
view model =
    Html.div [ Attributes.class "px-4" ]
        [ Html.div
            [ Attributes.class "text-center mx-auto text-center md:min-h-screen  flex flex-col justify-center"
            ]
            [ Html.h1
                [ Attributes.class "text-6xl mt-4 pt-4 md:-mt-24 font-light font-bold "
                ]
                [ Html.text "Mods in Bio" ]
            , Html.p [ Attributes.class "text-xl mt-2 mb-3" ]
                [ Html.text "Share links to your car's mods with your followers and fans." ]
            , Html.form [ Events.onSubmit SaveEmail ]
                [ Html.p [ Attributes.class "mt-12 mb-2" ] [ Html.text "Be the first to know when it's live." ]
                , Html.label []
                    [ Html.span [ Attributes.class "font-medium block sm:hidden mr-2 " ]
                        [ Html.text "Leave your email here ↓" ]
                    , Html.span [ Attributes.class "font-medium hidden sm:inline-block mr-2 " ]
                        [ Html.text "Leave your email here →" ]
                    , Html.input
                        [ Attributes.class "block sm:inline-block text-sm border px-2 py-2 w-full sm:w-1/4 mx-auto @todo font-semibold"
                        , Attributes.placeholder "hellofromus@modsinbio.com"
                        , model.maybeEmail
                            |> Maybe.map Attributes.value
                            |> Maybe.withDefault (Attributes.value "")
                        , Events.onInput SetEmail
                        ]
                        []
                    , Html.button
                        [ Attributes.class " px-4 py-2 font-medium text-center rounded-sm border w-full sm:w-32 mt-2 mb-1 sm:ml-2"
                        , Attributes.classList
                            [ ( "cursor-not-allowed opacity-75"
                              , model.savedEmail
                              )
                            ]
                        , Attributes.disabled model.savedEmail
                        ]
                        [ Html.text "Submit" ]
                    ]
                ]
            ]
        , Html.div
            [ Attributes.class "md:w-4/5 mx-auto  pt-12 pb-8"
            ]
            [ Html.div [ Attributes.class "text-center mb-12" ]
                [ Html.h2 [ Attributes.class "text-2xl font-semibold" ]
                    [ Html.text "Show off your upgrades." ]
                , Html.p []
                    [ Html.text "Everyone wants to know where to find your favorite mods."
                    ]
                ]
            , Html.div [ Attributes.class "md:flex pt-8" ]
                [ Html.div [ Attributes.class "flex-1 hidden md:block" ]
                    [ Html.div
                        [ Attributes.class "border-2 border-black mx-auto rounded-sm overflow-scroll"
                        , Attributes.style "width" "320px"
                        , Attributes.style "height" "529px"
                        ]
                        [ case model.profile of
                            Just { profile, mods } ->
                                Dict.toList mods
                                    |> List.map Tuple.second
                                    |> List.filter (.links >> List.isEmpty >> not)
                                    |> List.sortBy .order
                                    |> Profile.view profile
                                    |> Html.map ProfileMsg

                            Nothing ->
                                -- @TODO error handling
                                Html.text ""
                        ]
                    , Html.p
                        [ Attributes.class "font-monospace text-center mt-4"
                        , Attributes.style "font-family" "monospace"
                        ]
                        [ Html.a
                            [ Route.href <| Route.Profile "dwrxht"
                            , Attributes.target "_blank"
                            , Attributes.rel "noopener"
                            ]
                            [ Html.text "https://modsinbio.com/dwrxht" ]
                        ]
                    ]
                , Html.div [ Attributes.class "flex-1" ]
                    [ Html.h4 [ Attributes.class "text-lg mb-2 font-semibold" ] [ Html.text "How it works" ]
                    , Html.p [] [ Html.text "Sign up now for free." ]
                    , Html.p [] [ Html.text "Add links to your car's mods." ]
                    , Html.p []
                        [ Html.text "Update your instagram bio with the link to your mods."
                        , Html.p
                            [ Attributes.class "font-monospace text-lg "
                            , Attributes.style "font-family" "monospace"
                            ]
                            [ Html.a
                                [ Attributes.class "inline-block py-2 underline text-blue-500"
                                , Route.href <| Route.Profile "dwrxht"
                                ]
                                [ Html.text "https://modsinbio.com/dwrhxt" ]
                            ]
                        ]
                    , Html.p []
                        [ Html.text "Make it easy to find which parts and accessories youinstalled."
                        ]
                    , Html.p [] [ Html.text "Track views and link clicks." ]
                    , Html.p [] [ Html.text "Know which mods are most popular." ]
                    , Html.p [] [ Html.text "Who knows, maybe you'll even find a sponsor." ]
                    ]
                ]
            ]
        ]

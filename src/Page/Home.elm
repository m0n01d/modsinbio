module Page.Home exposing (..)

import Data.Session exposing (Session)
import Data.User as User
import Dict
import Html exposing (Html)
import Html.Attributes as Attributes
import Network.Api as Api
import Network.User as User
import Page.Profile as Profile
import Route
import Task


type alias Model =
    { session : Session
    , profile : Maybe User.PublicProfile
    }


type Msg
    = NoOp
    | ProfileMsg Profile.Msg
    | GotProfile (Result Api.Error User.PublicProfile)


init : Session -> String -> ( Model, Cmd Msg )
init session username =
    ( { session = session, profile = Nothing }
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

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div
            [ Attributes.class "text-center mx-auto text-center min-h-screen flex flex-col justify-center"
            ]
            [ Html.h1
                [ Attributes.class "text-3xl  -mt-24 font-light"
                ]
                [ Html.text "Mods in Bio" ]
            , Html.p [ Attributes.class "text-xl mt-2 mb-3" ] [ Html.text "Share links to your car's mods with your followers and fans." ]
            , Html.p
                [ Attributes.class "text-sm border px-2 py-2 w-32 mx-auto @todo font-semibold"
                ]
                [ Html.text "Sign up for free" ]
            ]
        , Html.div
            [ Attributes.class "md:w-4/5 mx-auto  px-4 pt-12 pb-8"
            ]
            [ Html.h2 [ Attributes.class "text-2xl text-center mb-12" ] [ Html.text "Show off your upgrades." ]
            , Html.div [ Attributes.class "flex" ]
                [ Html.div [ Attributes.class "flex-1 " ]
                    [ Html.p []
                        [ Html.text "Everyone wants to know where to find your favorite mods."
                        ]
                    , Html.div
                        [ Attributes.style "transform" "scale(0.75)"
                        , Attributes.class "border-2 border-black mx-auto rounded-sm"
                        , Attributes.style "width" "320px"
                        , Attributes.style "height" "529px"
                        ]
                        [ case model.profile of
                            Just { profile, mods } ->
                                let
                                    mods_ =
                                        Dict.toList mods
                                            |> List.map Tuple.second
                                            |> List.filter (.links >> List.isEmpty >> not)
                                            |> List.sortBy .order
                                in
                                Profile.view mods_ profile
                                    |> Html.map ProfileMsg

                            Nothing ->
                                -- @TODO error handling
                                Html.text ""
                        ]
                    ]
                , Html.div [ Attributes.class "flex-1" ]
                    [ Html.h4 [] [ Html.text "How it works" ]
                    , Html.p [] [ Html.text "Sign up now for free." ]
                    , Html.p [] [ Html.text "Add links to your car's mods." ]
                    , Html.p []
                        [ Html.text "Share your mods by adding the link to your instagram profile."
                        , Html.p
                            [ Attributes.class "font-monospace"
                            , Attributes.style "font-family" "monospace"
                            ]
                            [ Html.text "https://modsinbio.com/[your_instagram]" ]
                        ]
                    , Html.p [] [ Html.text "Please your fans by making it easy to find which parts and accessories are installed." ]
                    , Html.p [] [ Html.text "Track views and link clicks." ]
                    , Html.p [] [ Html.text "Know which mods are most popular." ]
                    , Html.p [] [ Html.text "Who knows, maybe you'll even find a sponsor." ]
                    ]
                ]
            ]
        ]

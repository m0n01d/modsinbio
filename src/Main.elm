module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Data.Session as Session
import Data.User as User
import Html exposing (Html, div, h1, img, text)
import Html.Attributes as Attributes exposing (src)
import Html.Events as Events
import Json.Decode as Decode exposing (Value)
import Json.Decode.Extra as Decode
import Json.Decode.Pipeline as Decode
import Network.Api as Api
import Network.User as User
import Page.Authed as Authed
import Page.Home as Home
import Page.Login as Login
import Page.MyMods as MyMods
import Page.Profile as Profile
import Page.Settings as Settings
import RemoteData exposing (WebData)
import Route exposing (Route)
import Task
import Url



---- MODEL ----


type Model
    = Home Home.Model
    | Login Login.Model
    | MyMods MyMods.Model
    | Settings Settings.Model
    | Redirect Session.Session
    | Authed Authed.Model
    | Profile Profile.Model


type alias DecodedFlags =
    { user : User.User
    }


type alias Flags =
    Value


decoder =
    Decode.succeed DecodedFlags
        |> Decode.required "user" User.decoder


decodeFlags : Flags -> Result Decode.Error DecodedFlags
decodeFlags flags =
    Decode.decodeValue decoder flags


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        decoded =
            decodeFlags flags
    in
    case decoded of
        Ok { user } ->
            Redirect { key = key, user = Just user }
                |> changeRouteTo (Route.fromUrl url)

        Err e ->
            -- let
            --     _ =
            --         Debug.log "why" e
            -- in
            Redirect { key = key, user = Nothing }
                |> changeRouteTo (Route.fromUrl url)



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg
    | MyModsMsg MyMods.Msg
    | AuthedMsg Authed.Msg
    | ProfileMsg Profile.Msg
    | SubmitLogin -- todo move to Login.elm
    | LoginResponse (WebData ())
    | SetEmail String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( UrlChanged url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( HomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel
                |> updateWith Home HomeMsg

        ( HomeMsg _, _ ) ->
            ( model, Cmd.none )

        ( MyModsMsg subMsg, MyMods subModel ) ->
            MyMods.update subMsg subModel
                |> updateWith MyMods MyModsMsg

        ( MyModsMsg _, _ ) ->
            ( model, Cmd.none )

        ( AuthedMsg subMsg, Authed subModel ) ->
            Authed.update subMsg subModel
                |> updateWith Authed AuthedMsg

        ( AuthedMsg _, _ ) ->
            ( model, Cmd.none )

        ( ProfileMsg subMsg, Profile subModel ) ->
            Profile.update subMsg subModel
                |> updateWith Profile ProfileMsg

        ( ProfileMsg _, _ ) ->
            ( model, Cmd.none )

        ( SubmitLogin, Login subModel ) ->
            ( Login { subModel | loginResponse = RemoteData.Loading }, User.login subModel.email LoginResponse )

        ( LoginResponse res, Login _ ) ->
            ( Login { session = toSession model, email = "", loginResponse = res }, Cmd.none )

        ( SetEmail email, Login subModel ) ->
            ( Login { session = subModel.session, email = email, loginResponse = RemoteData.NotAsked }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


toSession : Model -> Session.Session
toSession model =
    case model of
        Home { session } ->
            session

        Login { session } ->
            session

        MyMods { session } ->
            session

        Settings { session } ->
            session

        Redirect session ->
            session

        Profile { session } ->
            session

        Authed { session } ->
            session


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case ( maybeRoute, session.user ) of
        ( Just (Route.Profile username), _ ) ->
            Profile.init session username
                |> Tuple.mapFirst Profile
                |> Tuple.mapSecond (Cmd.map ProfileMsg)

        ( Just Route.Home, _ ) ->
            Home.init session "dwrxht"
                |> Tuple.mapFirst Home
                |> Tuple.mapSecond (Cmd.map HomeMsg)

        ( Nothing, _ ) ->
            -- todo
            toHome session

        ( Just Route.Login, _ ) ->
            ( Login { session = session, email = "", loginResponse = RemoteData.NotAsked }, Cmd.none )

        ( Just (Route.Authed payload), _ ) ->
            case payload of
                Just token ->
                    Authed.init session token
                        |> Tuple.mapFirst Authed
                        |> Tuple.mapSecond (Cmd.map AuthedMsg)

                Nothing ->
                    toHome session

        ( _, Nothing ) ->
            toHome session

        ( Just Route.Settings, _ ) ->
            ( Settings { session = session }, Cmd.none )

        ( Just Route.Admin, _ ) ->
            case session.user of
                Nothing ->
                    toHome session

                Just user ->
                    case user of
                        User.Driver (User.DriverFull token profile) ->
                            MyMods.init session token profile
                                |> Tuple.mapFirst MyMods
                                |> Tuple.mapSecond (Cmd.map MyModsMsg)

                        User.Partial token ->
                            -- to authed? -- would this loop???
                            Authed.init session token
                                |> Tuple.mapFirst Authed
                                |> Tuple.mapSecond (Cmd.map AuthedMsg)


toHome session =
    ( Home <| Home.initialModel session
    , Nav.replaceUrl session.key (Route.routeToString Route.Home)
    )



---- VIEW ----


view : Model -> Document Msg
view model =
    let
        { title, content } =
            viewContent model
    in
    { title = title
    , body =
        [ div [ Attributes.class "container mx-auto root flex flex-col min-h-screen" ]
            [ navbar model
            , Html.main_ [ Attributes.class "flex-1" ] [ content ]
            ]
        ]
    }


navbar model =
    case model of
        Profile _ ->
            Html.text ""

        _ ->
            Html.header [ Attributes.class "h-6 py-8 px-4 border-b border-grey-500 flex items-center" ]
                [ Html.a [ Route.href Route.Home ] [ Html.text "Mods in Bio" ]
                , Html.a [ Attributes.class "ml-auto", Route.href Route.Login ] [ Html.text "Login" ]
                ]


viewContent : Model -> { title : String, content : Html Msg }
viewContent model =
    case model of
        Home m ->
            { title = "Mods in Bio"
            , content =
                Home.view m
                    |> Html.map HomeMsg
            }

        MyMods subModel ->
            { title = " My Mods - Mods in Bio"
            , content =
                MyMods.view subModel
                    |> Html.map MyModsMsg
            }

        Profile subModel ->
            let
                { title, content } =
                    Profile.page subModel
            in
            { title = title
            , content = Html.map ProfileMsg content
            }

        Login { email, loginResponse } ->
            { title = "Login"
            , content =
                let
                    form isLoading =
                        Html.form [ Attributes.class "mx-auto md:w-1/4", Attributes.disabled isLoading, Events.onSubmit SubmitLogin ]
                            [ Html.div []
                                [ Html.label [ Attributes.class "text-sm font-medium mb-2" ] [ Html.text "Your email" ]
                                , Html.input
                                    [ Attributes.class "px-2 py-1 block border w-full"
                                    , Attributes.placeholder "bruce@wayneindustries.com"
                                    , Events.onInput SetEmail
                                    , Attributes.value email
                                    ]
                                    []
                                , Html.button [ Attributes.class "w-full mt-2 px-4 py-2 font-medium text-center rounded-sm border mr-4" ]
                                    [ Html.text "Submit" ]
                                ]
                            , Html.p [ Attributes.class "mt-4 text-sm text-gray-700" ] [ Html.text "We'll email you a link to sign in." ]
                            ]
                in
                Html.div []
                    [ Html.div [ Attributes.class "w-full mx-auto mt-12 md:mt-32" ]
                        [ case loginResponse of
                            RemoteData.NotAsked ->
                                form False

                            RemoteData.Success () ->
                                Html.div [ Attributes.class "w-1/3 mx-auto" ]
                                    [ Html.p [] [ Html.text "Now would be a good time to check your inbox." ]
                                    , Html.p [] [ Html.text "If an email isn't in there in 5 minutes, read this message again." ]
                                    ]

                            RemoteData.Loading ->
                                form True

                            RemoteData.Failure why ->
                                Html.text "An error occured"
                        ]
                    ]
            }

        _ ->
            { title = "x", content = Html.text "" }



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }

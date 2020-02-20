module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Data.Session as Session
import Data.User as User
import Html exposing (Html, div, h1, img, text)
import Html.Attributes as Attributes exposing (src)
import Json.Decode as Decode exposing (Value)
import Json.Decode.Extra as Decode
import Json.Decode.Pipeline as Decode
import Network.Api as Api
import Page.Authed as Authed
import Page.Home as Home
import Page.Login as Login
import Page.MyMods as MyMods
import Page.Profile as Profile
import Page.Settings as Settings
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
        |> Decode.required "user" User.decodeDriver


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
            Redirect { key = key, user = user }
                |> changeRouteTo (Route.fromUrl url)

        Err e ->
            -- let
            --     _ =
            --         Debug.log "why" e
            -- in
            Redirect { key = key, user = User.Public }
                |> changeRouteTo (Route.fromUrl url)



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg
    | MyModsMsg MyMods.Msg
    | AuthedMsg Authed.Msg
    | ProfileMsg Profile.Msg


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
            ( Home <| Home.initialModel session, Cmd.none )

        ( Just Route.Login, _ ) ->
            ( Login { session = session }, Cmd.none )

        ( Just (Route.Authed payload), _ ) ->
            case payload of
                Just token ->
                    Authed.init session token
                        |> Tuple.mapFirst Authed
                        |> Tuple.mapSecond (Cmd.map AuthedMsg)

                Nothing ->
                    -- todo redirect home?
                    ( model, Cmd.none )

        ( _, User.Public ) ->
            ( Home <| Home.initialModel session, Nav.replaceUrl session.key (Route.routeToString Route.Home) )

        ( Just Route.Settings, _ ) ->
            ( Settings { session = session }, Cmd.none )

        ( Just Route.Admin, _ ) ->
            case session.user of
                User.Driver token profile ->
                    MyMods.update MyMods.InitializeMyMods (MyMods.initialModel session profile)
                        |> Tuple.mapFirst MyMods
                        |> Tuple.mapSecond (Cmd.map MyModsMsg)

                _ ->
                    ( model, Cmd.none )



---- VIEW ----


view : Model -> Document Msg
view model =
    let
        { title, content } =
            viewContent model
    in
    { title = title
    , body =
        [ div [ Attributes.class "container mx-auto root flex flex-col" ]
            [ navbar model
            , Html.main_ [ Attributes.class "px-2 flex-1" ] [ content ]
            ]
        ]
    }


navbar model =
    Html.header [ Attributes.class "h-6 py-8 px-2 border-b border-grey-500 flex items-center" ]
        [ Html.text "Navbar"
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

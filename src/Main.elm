module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Data.Session as Session
import Data.User as User
import Html exposing (Html, div, h1, img, text)
import Html.Attributes as Attributes exposing (src)
import Json.Decode as Decode
import Json.Decode.Extra as Decode
import Network.Api as Api
import Page.Home as Home
import Page.Login as Login
import Page.MyMods as MyMods
import Page.Settings as Settings
import Route exposing (Route)
import Task
import Url



---- MODEL ----


type Model
    = Home Home.Model
    | Login Login.Model
    | MyMods MyMods.Model
    | Profile Session.Session String
    | Settings Settings.Model
    | Redirect Session.Session


type alias Flags =
    Decode.Value


decodeFlags str =
    Decode.decodeValue User.decodeDriver str
        |> Result.withDefault User.Public


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    Redirect { key = key, user = decodeFlags flags }
        |> changeRouteTo (Route.fromUrl url)



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg
    | MyModsMsg MyMods.Msg
    | ReceivedAuth (Result Api.Error User.DriverProfile)


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

        ( ReceivedAuth (Ok user), _ ) ->
            let
                oldSession =
                    toSession model

                session =
                    { oldSession | user = User.driverPartialToFull oldSession.user user }

                model_ =
                    Redirect session
            in
            ( model_
            , Nav.replaceUrl session.key (Route.routeToString Route.Admin)
              -- @TODO SAVE USER TO PORTS AND REDIRECT
            )

        ( ReceivedAuth _, _ ) ->
            ( model
            , Cmd.none
              -- @TODO SAVE USER TO PORTS AND REDIRECT
            )


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

        Profile session _ ->
            session


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
    case maybeRoute of
        Nothing ->
            ( Home { session = session }, Cmd.none )

        Just Route.Home ->
            ( Home { session = session }, Cmd.none )

        Just Route.Login ->
            ( Login { session = session }, Cmd.none )

        Just Route.Admin ->
            MyMods.update MyMods.InitializeMyMods (MyMods.initialModel session)
                |> Tuple.mapFirst MyMods
                |> Tuple.mapSecond (Cmd.map MyModsMsg)

        Just Route.Settings ->
            ( Settings { session = session }, Cmd.none )

        Just (Route.Authed payload) ->
            case payload of
                Just token ->
                    let
                        fetchUser =
                            User.query session token
                                |> Task.attempt ReceivedAuth
                    in
                    ( Redirect { session | user = User.DriverPartial token }, fetchUser )

                Nothing ->
                    ( model, Cmd.none )

        Just (Route.Profile username) ->
            ( Profile session username, Cmd.none )



---- VIEW ----


view : Model -> Document Msg
view model =
    let
        { title, content } =
            viewContent model
    in
    { title = title
    , body =
        [ div [ Attributes.class "container mx-auto" ]
            [ navbar model
            , Html.main_ [ Attributes.class "pt-12 px-2" ] [ content ]
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
            { title = "Mods in Bio", content = Home.view }

        MyMods subModel ->
            { title = " My Mods - Mods in Bio"
            , content =
                MyMods.view subModel
                    |> Html.map MyModsMsg
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

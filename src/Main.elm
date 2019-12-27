module Main exposing (..)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Data.Session as Session
import Html exposing (Html, div, h1, img, text)
import Html.Attributes as Attributes exposing (src)
import Page.Home as Home
import Page.Login as Login
import Page.MyMods as MyMods
import Page.Settings as Settings
import Route exposing (Route)
import Url



---- MODEL ----


type Model
    = Home Home.Model
    | Login Login.Model
    | MyMods MyMods.Model
    | Settings Settings.Model


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ key =
    ( Home { session = { key = key } }, Cmd.none )



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            changeRouteTo (Route.fromUrl url) model

        NoOp ->
            ( model, Cmd.none )


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
            ( MyMods { session = session }, Cmd.none )

        Just Route.Settings ->
            ( Settings { session = session }, Cmd.none )



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "hi"
    , body =
        [ div [ Attributes.class "container mx-auto" ]
            [ h1 [] [ text "Your Elm App is workings!" ]
            , MyMods.view
            ]
        ]
    }



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = always NoOp
        , onUrlRequest = always NoOp
        }

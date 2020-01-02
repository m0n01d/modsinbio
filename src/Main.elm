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
    | Redirect Session.Session


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    Redirect { key = key }
        |> changeRouteTo (Route.fromUrl url)



---- UPDATE ----


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | HomeMsg Home.Msg
    | MyModsMsg MyMods.Msg


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
            ( MyMods <| MyMods.initialModel session
            , Cmd.none
            )

        Just Route.Settings ->
            ( Settings { session = session }, Cmd.none )



---- VIEW ----


view : Model -> Document Msg
view model =
    { title = "hi"
    , body =
        [ div [ Attributes.class "container mx-auto" ]
            [ navbar model
            , viewContent model
            ]
        ]
    }


navbar model =
    Html.header [ Attributes.class "h-6 py-4 px-2 border-b border-grey-500 flex items-center" ]
        [ Html.text "Navbar"
        ]


viewContent : Model -> Html Msg
viewContent model =
    Html.main_ [ Attributes.class "mt-4 px-2" ]
        [ case model of
            Home m ->
                Home.view

            MyMods subModel ->
                MyMods.view subModel
                    |> Html.map MyModsMsg

            _ ->
                Html.text ""
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }

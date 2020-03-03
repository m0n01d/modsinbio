module Page.Authed exposing (..)

import Browser.Navigation as Nav
import Data.Session as Session exposing (Session)
import Data.User as User
import Json.Encode as Encode exposing (Value)
import Network.Api as Api
import Network.User as User
import Route
import Task


type alias Model =
    { session : Session.Session }


type Msg
    = ReceivedAuth String (Result Api.Error User.DriverProfile)


init : Session -> String -> ( Model, Cmd Msg )
init oldSession token =
    let
        fetchUser =
            User.query oldSession token
                |> Task.attempt (ReceivedAuth token)
    in
    ( { session = { oldSession | user = Just <| User.Partial token } }, fetchUser )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReceivedAuth token (Ok driverProfile) ->
            -- sve to port
            -- update session
            -- i guess in here would be where you fetch the user, check its 'role'
            -- and determine which User variant to apply
            let
                oldSession =
                    model.session

                session =
                    { oldSession
                        | user =
                            oldSession.user
                                |> Maybe.map
                                    (User.driverPartialToFull driverProfile)
                    }
            in
            ( { model | session = session }
            , Cmd.batch
                [ --save to port
                  -- redirect
                  Nav.replaceUrl session.key (Route.routeToString Route.Admin)
                , Session.saveUser token driverProfile
                ]
            )

        ReceivedAuth _ (Err err) ->
            -- let
            --     _ =
            --         Debug.log "todo " err
            -- in
            ( model, Cmd.none )

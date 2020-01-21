port module Page.Authed exposing (..)

import Browser.Navigation as Nav
import Data.Session as Session
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


init oldSession token =
    let
        fetchUser =
            User.query oldSession token
                |> Task.attempt (ReceivedAuth token)
    in
    ( { session = { oldSession | user = User.DriverPartial token } }, fetchUser )


update msg model =
    case msg of
        ReceivedAuth token (Ok driverProfile) ->
            -- sve to port
            -- update session
            let
                oldSession =
                    model.session

                session =
                    { oldSession | user = User.driverPartialToFull oldSession.user driverProfile }

                encodedUser =
                    User.encodeDriver token driverProfile

                payload =
                    Encode.object [ ( "payload", encodedUser ), ( "tag", Encode.string "SAVE_USER" ) ]
            in
            ( { model | session = session }
            , Cmd.batch
                [ --save to port
                  -- redirect
                  Nav.replaceUrl session.key (Route.routeToString Route.Admin)
                , toJs payload
                ]
            )

        ReceivedAuth _ (Err err) ->
            -- let
            --     _ =
            --         Debug.log "todo " err
            -- in
            ( model, Cmd.none )


port toJs : Value -> Cmd msg

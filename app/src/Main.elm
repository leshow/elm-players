module Main exposing (..)

import Navigation
import Models exposing (Model, initialModel)
import Messages exposing (Msg(..))
import View exposing (view)
import Update exposing (update)
import Players.Commands exposing (fetchPlayers)
import Routing exposing (Route)


init : Navigation.Location -> ( Model, Cmd Msg )
init result =
    let
        currentRoute =
            Routing.parseLocation result
    in
        ( initialModel currentRoute, Cmd.map PlayersMsg fetchPlayers )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

module Players.Update exposing (..)

import Players.Messages exposing (Msg(..))
import Players.Models exposing (Player)
import Navigation


update : Msg -> List Player -> ( List Player, Cmd Msg )
update action players =
    case action of
        NoOp ->
            players ! []

        FetchSuccess playerList ->
            playerList ! []

        FetchError error ->
            players ! []

        ShowPlayers ->
            ( players, Navigation.modifyUrl "#players" )

        ShowPlayer id ->
            ( players, Navigation.modifyUrl ("#players/" ++ (toString id)) )

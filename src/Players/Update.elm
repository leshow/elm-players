module Players.Update exposing (..)

import Players.Messages exposing (Msg(..))
import Players.Models exposing (Player, PlayerId)
import Players.Commands exposing (save)
import Navigation


update : Msg -> List Player -> ( List Player, Cmd Msg )
update action players =
    case action of
        NoOp ->
            players ! []

        PlayerListFetch (Ok playerList) ->
            playerList ! []

        PlayerListFetch (Err error) ->
            players ! []

        ShowPlayers ->
            ( players, Navigation.modifyUrl "#players" )

        ShowPlayer id ->
            ( players, Navigation.modifyUrl ("#players/" ++ (toString id)) )

        ChangeLevel id howMuch ->
            ( players
            , changeLevelCommands id howMuch players
                |> Cmd.batch
            )

        PlayerSave (Ok newPlayer) ->
            ( updatePlayer newPlayer players, Cmd.none )

        PlayerSave (Err err) ->
            ( players, Cmd.none )



-- the list returned here will be a save Command of all the players with a pid matching the
-- first argument


changeLevelCommands : PlayerId -> Int -> List Player -> List (Cmd Msg)
changeLevelCommands pid howMuch players =
    let
        playerCmd currentPlayer =
            if currentPlayer.id == pid then
                save { currentPlayer | level = currentPlayer.level + howMuch }
            else
                Cmd.none
    in
        List.map playerCmd players


updatePlayer : Player -> List Player -> List Player
updatePlayer player players =
    let
        changePlayer currentPlayer =
            if currentPlayer.id == player.id then
                player
                -- swap currentPlayer in list with our passed player
            else
                currentPlayer
    in
        List.map (\x -> changePlayer x) players

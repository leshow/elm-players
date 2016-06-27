module View exposing (..)

import Html.App
import Html exposing (Html, div, text)
import Models exposing (Model)
import Messages exposing (Msg(..))
import Players.List
import Players.Edit
import Routing exposing (..)
import Players.Models exposing (PlayerId)


view : Model -> Html Msg
view model =
    div [] [ page model ]


page : Model -> Html Msg
page model =
    case model.route of
        PlayersRoute ->
            Html.App.map PlayersMsg (Players.List.view model.players)

        RouteNotFound ->
            notFoundView

        PlayerRoute id ->
            playerEditView model id


playerEditView : Model -> PlayerId -> Html Msg
playerEditView model playerId =
    let
        maybePlayer =
            model.players
                |> List.filter (\player -> player.id == playerId)
                |> List.head
    in
        case maybePlayer of
            Just player ->
                Html.App.map PlayersMsg (Players.Edit.view player)

            Nothing ->
                notFoundView


notFoundView : Html Msg
notFoundView =
    div [] [ text "Cant find that player" ]

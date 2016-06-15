module View exposing (..)

import Html.App
import Html exposing (Html, div, text)
import Models exposing (Model)
import Messages exposing (Msg(..))
import Players.List


view : Model -> Html Msg
view model =
    div [] [ page model ]


page : Model -> Html Msg
page model =
    Html.App.map PlayersMsg (Players.List.view model.players)

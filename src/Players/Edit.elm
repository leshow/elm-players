module Players.Edit exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, value, href)
import Players.Models exposing (..)
import Players.Messages exposing (..)


view : Player -> Html.Html Msg
view model =
    div []
        [ nav model
        , form model
        ]


nav : Player -> Html.Html Msg
nav player =
    div [ class "clearfix mb2 white bg-black p1" ]
        [ btnPlayerList ]


form : Player -> Html.Html Msg
form player =
    div [ class "m3" ]
        [ h1 [] [ text player.name ]
        , formLevel player
        ]


formLevel : Player -> Html.Html Msg
formLevel player =
    div [ class "clearfix py1" ]
        [ div [ class "col col-5" ] [ text "Level" ]
        , div [ class "col col-7" ]
            [ span [ class "h2 bold" ] [ text (toString player.level) ]
            , btnDropLevel player
            , btnRaiseLevel player
            ]
        ]


btnDropLevel : Player -> Html.Html Msg
btnDropLevel player =
    a [ class "btn ml1 h1", onClick (ChangeLevel player.id -1) ]
        [ i [ class "fa fa-minus-circle" ] [] ]


btnRaiseLevel : Player -> Html.Html Msg
btnRaiseLevel player =
    a [ class "btn ml1 h1", onClick (ChangeLevel player.id 1) ]
        [ i [ class "fa fa-plus-circle" ] [] ]


btnPlayerList : Html Msg
btnPlayerList =
    button
        [ class "btn regular"
        , onClick ShowPlayers
        ]
        [ i [ class "fa fa-chevron-left mr1" ] [], text "List" ]

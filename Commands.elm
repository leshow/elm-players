module Commands exposing (..)

import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Html.App
import Random


type alias RandomNum =
    Int


type Msg
    = Roll
    | OnResult Int


init : ( RandomNum, Cmd Msg )
init =
    1 ! []


view : RandomNum -> Html Msg
view model =
    div []
        [ button [ onClick Roll ] [ text "Roll!" ]
        , div [] [ text <| toString model ]
        ]


update : Msg -> RandomNum -> ( RandomNum, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, Random.generate OnResult <| Random.int 0 100 )

        OnResult result ->
            result ! []


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }

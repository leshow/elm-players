module Main exposing (..)

import Html exposing (Html, div, text, button)
import Html.App exposing (map)
import Widget as Counter


type alias AppModel =
    { counterModel : Counter.Model }


initialModel : AppModel
initialModel =
    { counterModel = Counter.initialModel }


init : ( AppModel, Cmd Msg )
init =
    ( initialModel, Cmd.none )


type Msg
    = CounterMsg Counter.Msg


view : AppModel -> Html Msg
view model =
    div []
        [ map CounterMsg (Counter.view model.counterModel)
        , div []
            [ text
                <| toString
                <| Counter.count model.counterModel
            ]
        ]


update : Msg -> AppModel -> ( AppModel, Cmd Msg )
update msg model =
    case msg of
        CounterMsg cmsg ->
            let
                ( cmodel, ccmd ) =
                    (Counter.update cmsg model.counterModel)
            in
                ( { model | counterModel = cmodel }, Cmd.map CounterMsg ccmd )


subscriptions : AppModel -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

module Main exposing (..)

import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)
import Html.App


type alias Model =
    Int


init : ( Model, Cmd Msg )
init =
    ( 0, Cmd.none )


type Msg
    = Increment Int
    | Decrement Int


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick (Increment 2) ] [ text "Inc 2" ]
        , button [ onClick (Decrement 1) ] [ text "Dec 1" ]
        , text (toString model)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment amount ->
            ( model + amount, Cmd.none )

        Decrement amount ->
            ( model - amount, Cmd.none )


subscriptions : Model -> Sub Msg
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

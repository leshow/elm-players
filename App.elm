module Main exposing (..)

import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)
import Html.App


type alias Model =
    Bool


init : ( Model, Cmd Msg )
init =
    ( False, Cmd.none )


type Msg
    = Expand
    | Collapse


view : Model -> Html Msg
view model =
    case model of
        True ->
            div []
                [ button [ onClick Collapse ] [ text "Collapse" ]
                , text (toString model)
                ]

        False ->
            div [] [ button [ onClick Expand ] [ text "Expand" ] ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Expand ->
            ( True, Cmd.none )

        Collapse ->
            ( False, Cmd.none )


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

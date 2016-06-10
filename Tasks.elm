module Tasks exposing (..)

import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Html.App
import Http
import Task exposing (Task)
import Json.Decode as Decode


type alias Model =
    String


type Msg
    = Fetch
    | FetchSuccess String
    | FetchError Http.Error


init : ( Model, Cmd Msg )
init =
    "" ! []


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Fetch ] [ text "Get data" ]
        , text model
        ]


decodeName : Decode.Decoder String
decodeName =
    Decode.at [ "name" ] Decode.string


url : String
url =
    "http://swapi.com/api/planets/1/"


fetchTask : Task Http.Error String
fetchTask =
    Http.get decodeName url


fetchCmd : Cmd Msg
fetchCmd =
    Task.perform FetchError FetchSuccess fetchTask


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Fetch ->
            ( model, fetchCmd )

        FetchSuccess name ->
            name ! []

        FetchError error ->
            model ! []


main : Program Never
main =
    Html.App.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }

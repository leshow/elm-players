module Widget exposing (..)

import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)


type alias Model =
    { count : Int }


initialModel : Model
initialModel =
    { count = 0 }


type Msg
    = Increment
    | Decrement


count : Model -> Int
count model =
    model.count


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Increment ] [ text "Inc" ]
        , button [ onClick Decrement ] [ text "Dec" ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Increment ->
            ( { model | count = model.count + 1 }, Cmd.none )

        Decrement ->
            ( { model | count = model.count - 1 }, Cmd.none )

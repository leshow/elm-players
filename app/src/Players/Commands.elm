module Players.Commands exposing (..)

import Http
import Json.Decode as Decode exposing ((:=))
import Task
import Players.Models exposing (PlayerId, Player)
import Players.Messages exposing (..)


collectionDecoder : Decode.Decoder (List Player)
collectionDecoder =
    Decode.list playerDecode


playerDecode : Decode.Decoder Player
playerDecode =
    Decode.object3 Player
        ("id" := Decode.int)
        ("name" := Decode.string)
        ("level" := Decode.int)


playerUrl : String
playerUrl =
    "http://localhost:4000/players"


fetchTask : Task.Task Http.Error (List Player)
fetchTask =
    Http.get collectionDecoder playerUrl


fetchPlayers : Cmd Msg
fetchPlayers =
    Task.perform FetchError FetchSuccess fetchTask

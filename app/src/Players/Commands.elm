module Players.Commands exposing (..)

import Http
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Task
import Players.Models exposing (PlayerId, Player)
import Players.Messages exposing (..)


saveUrl : PlayerId -> String
saveUrl playerId =
    "http://localhost:4000/players" ++ (toString playerId)


saveTask : Player -> Task.Task Http.Error Player
saveTask player =
    let
        body =
            memberEncoded player
                |> Encode.encode 0
                |> Http.string

        config =
            { verb = "PATCH"
            , headers = [ ( "Content-Type", "application/json" ) ]
            , url = saveUrl player.id
            , body = body
            }
    in
        Http.send Http.defaultSettings config
            |> Http.fromJson memberDecoder


save : Player -> Cmd Msg
save player =
    saveTask player
        |> Task.perform SaveFail SaveSuccess


memberEncoded : Player -> Encode.Value
memberEncoded player =
    let
        list =
            [ ( "id", Encode.int player.id )
            , ( "name", Encode.string player.name )
            , ( "level", Encode.int player.level )
            ]
    in
        list
            |> Encode.object


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

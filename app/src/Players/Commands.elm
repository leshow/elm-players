module Players.Commands exposing (..)

import Http
import Json.Decode as Decode exposing ((:=))
import Json.Encode as Encode
import Task
import Players.Models exposing (PlayerId, Player)
import Players.Messages exposing (..)


saveUrl : PlayerId -> String
saveUrl playerId =
    "http://localhost:4000/api/player" ++ (toString playerId)


saveTask : Player -> Task.Task Http.Error Player
saveTask player =
    let
        body =
            playerEncoded player
                |> Encode.encode 0
                |> Http.string

        config =
            { verb = "POST"
            , headers = [ ( "Content-Type", "application/json" ) ]
            , url = saveUrl player.id
            , body = body
            }
    in
        Http.send Http.defaultSettings config
            |> Http.fromJson playerDecode


save : Player -> Cmd Msg
save player =
    saveTask player
        |> Task.perform SaveFail SaveSuccess


playerEncoded : Player -> Encode.Value
playerEncoded player =
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
    "http://localhost:4000/api/player"


fetchTask : Task.Task Http.Error (List Player)
fetchTask =
    Http.get collectionDecoder playerUrl


fetchPlayers : Cmd Msg
fetchPlayers =
    Task.perform FetchError FetchSuccess fetchTask

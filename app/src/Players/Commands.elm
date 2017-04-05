module Players.Commands exposing (..)

import Http
import Json.Decode as Decode exposing (field)
import Json.Encode as Encode
import Players.Models exposing (PlayerId, Player)
import Players.Messages exposing (..)


(:=) : String -> Decode.Decoder a -> Decode.Decoder a
(:=) =
    field


saveUrl : PlayerId -> String
saveUrl playerId =
    "http://localhost:4000/api/player/" ++ (toString playerId)


savePlayerRequest : Player -> Http.Request Player
savePlayerRequest player =
    let
        config =
            { method = "PUT"
            , headers = []
            , url = saveUrl player.id
            , body = Http.jsonBody <| playerEncoded player
            , expect = Http.expectJson playerDecode
            , timeout = Nothing
            , withCredentials = False
            }
    in
        Http.request config


save : Player -> Cmd Msg
save player =
    savePlayerRequest player
        |> Http.send PlayerSave


playerEncoded : Player -> Encode.Value
playerEncoded player =
    let
        list =
            [ ( "playerId", Encode.int player.id )
            , ( "playerName", Encode.string player.name )
            , ( "playerLevel", Encode.int player.level )
            ]
    in
        list
            |> Encode.object


collectionDecoder : Decode.Decoder (List Player)
collectionDecoder =
    Decode.list playerDecode


playerDecode : Decode.Decoder Player
playerDecode =
    Decode.map3 Player
        ("playerId" := Decode.int)
        ("playerName" := Decode.string)
        ("playerLevel" := Decode.int)


playerUrl : String
playerUrl =
    "http://localhost:4000/api/player"


fetchRequest : Http.Request (List Player)
fetchRequest =
    Http.get playerUrl collectionDecoder


fetchPlayers : Cmd Msg
fetchPlayers =
    fetchRequest |> Http.send PlayerListFetch

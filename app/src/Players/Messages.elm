module Players.Messages exposing (..)

import Http
import Players.Models exposing (Player, PlayerId)


type Msg
    = NoOp
    | PlayerListFetch (Result Http.Error (List Player))
    | ShowPlayers
    | ShowPlayer PlayerId
    | ChangeLevel PlayerId Int
    | PlayerSave (Result Http.Error Player)

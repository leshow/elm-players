module Players.Messages exposing (..)

import Http
import Players.Models exposing (Player, PlayerId)


type Msg
    = NoOp
    | FetchSuccess (List Player)
    | FetchError Http.Error
    | ShowPlayers
    | ShowPlayer PlayerId

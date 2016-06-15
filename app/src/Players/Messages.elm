module Players.Messages exposing (..)

import Http
import Players.Models exposing (Player)


type Msg
    = NoOp
    | FetchSuccess (List Player)
    | FetchError Http.Error

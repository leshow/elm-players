module Models exposing (..)

import Players.Models exposing (Player, PlayerId, new)


type alias Model =
    { players : List Player
    }


initialModel : Model
initialModel =
    { players = [ Player 1 "Evan" 1 ] }

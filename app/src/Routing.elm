module Routing exposing (..)

import Navigation
import UrlParser exposing (..)
import Players.Models exposing (PlayerId)


type Route
    = PlayersRoute
    | PlayerRoute PlayerId
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map PlayersRoute top
        , map PlayerRoute (s "players" </> int)
        , map PlayersRoute (s "players")
        ]


parseLocation : Navigation.Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute

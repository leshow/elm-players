# Concurrent API in Servant

Servant exposes a great type-safe way of writing api's. The requests run concurrently so it is necessary to lock/unlock data with MVar or STM.

I have used a mocked database in this simple example which is just a `Data.Map`.

This project was a great learning experience.

```
$ stack setup
$ stack build
$ stack exec server-exe
# in another terminal
$ curl http://localhost:4000/api/player
$ curl -X POST -d '{"playerName":"Plop", "playerLevel":9000, "playerId":5}' -H 'Content-type: application/json' http://localhost:4000/api/player/5
$ curl http://localhost:4000/api/player/5
$ curl --request PUT -d '{"playerName":"superduper", "playerLevel":9000, "playerId":5}' -H 'Content-type: application/json' http://localhost:4000/api/player/5

```

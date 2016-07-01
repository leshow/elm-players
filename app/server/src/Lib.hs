{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( start
    ) where

import Control.Concurrent
import Control.Monad.Trans.Except
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO
import Api
import Data.Map as M

start :: IO ()
start = do
    let port = 4000
        settings =
            setPort port $
            setBeforeMainLoop (hPutStrLn stderr
                ("listening on port " `mappend` show port)) $
            defaultSettings
    runSettings settings =<< app

app :: IO Application
app = return $ serve api server


server :: Server Api
server =
    getPlayers :<|>
    getPlayerById

getPlayers :: Handler [Player]
getPlayers = return $ existingPlayers

getPlayerById :: PlayerId -> Handler Player
getPlayerById id =
    let
        found = Prelude.filter (\x -> playerId x == id) existingPlayers
        isFound = not (Prelude.null found)
    in
        if isFound then return $ head found else throwE err404

existingPlayers :: [Player]
existingPlayers =
    [ Player 1 "Sally" 2
    , Player 2 "Lance" 1
    , Player 3 "Aki" 3
    , Player 4 "Maria" 4
    ]

-- player map
data DB = DB (MVar PlayerMap)

type PlayerMap = M.Map PlayerId Player

playerDB :: IO DB
playerDB = DB <$> newMVar M.empty -- <$> is also acceptable here

insertPlayer :: DB -> Player -> IO ()
insertPlayer (DB mvar) player = do
    pmap <- takeMVar mvar
    putMVar mvar (M.insert (playerId player) player pmap) -- insert player at { playerId: Player }

findPlayer :: DB -> PlayerId -> IO (Maybe Player)
findPlayer (DB mvar) idx = do
    pmap <- takeMVar mvar
    putMVar mvar pmap
    return (M.lookup idx pmap)

data Logger = Logger (MVar LogCommand)
data LogCommand = Log String | End (MVar ())

startLogger :: IO Logger
startLogger = do
    m <- newEmptyMVar
    let l = Logger m
    forkIO (logger l)
    return l

logger :: Logger -> IO ()
logger (Logger mvar) = do
    log <- takeMVar mvar
    case log of
        Log str -> do
            putStrLn str
            logger (Logger mvar)
        End m -> do
            putStrLn "Logger ending"
            putMVar m ()

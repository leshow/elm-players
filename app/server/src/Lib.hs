{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( start
    ) where

import Control.Concurrent
import Control.Monad.IO.Class
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
app = serve api <$> getServer

getServer :: IO (Server Api)
getServer = do
    db <- playerDB
    return $ server db

-- server :: Server Api
-- server =
--     getPlayers :<|>
--     getPlayerById
--
-- getPlayers :: Handler [Player]
-- getPlayers = return $ existingPlayers
--
-- getPlayerById :: PlayerId -> Handler Player
-- getPlayerById id =
--     let
--         found = Prelude.filter (\x -> playerId x == id) existingPlayers
--         isFound = not (Prelude.null found)
--     in
--         if isFound then return $ head found else throwE err404

-- existingPlayers :: [Player]
-- existingPlayers =
--     [ Player 1 "Sally" 2
--     , Player 2 "Lance" 1
--     , Player 3 "Aki" 3
--     , Player 4 "Maria" 4
--     ]

server :: DB -> Server Api
server db =
    getPlayers db :<|>
    getPlayerById db :<|>
    postPlayer db
    --updatePlayerById db

getPlayers :: DB -> Handler [Player]
getPlayers db = liftIO $ getPmap db

getPlayerById :: DB -> PlayerId -> Handler Player
getPlayerById db id = maybe (throwE err404) return =<< liftIO (findPlayer db id)

postPlayer :: DB -> PlayerId -> Player -> Handler Player
postPlayer db id player = liftIO $ insertPlayer db player


-- player map
data DB = DB (MVar PlayerMap)

type PlayerMap = M.Map PlayerId Player


existingPlayers :: [(PlayerId, Player)]
existingPlayers =
    [ (1, Player 1 "Sally" 2)
    , (2, Player 2 "Lance" 1)
    , (3, Player 3 "Aki" 3)
    , (4, Player 4 "Maria" 4)
    ]

getPmap :: DB -> IO [Player]
getPmap (DB mvar) = elem <$> readMVar mvar

playerDB :: IO DB
playerDB = DB <$> newMVar (M.fromList existingPlayers) -- <$> is also acceptable here

insertPlayer :: DB -> Player -> IO ()
insertPlayer (DB mvar) player = do
    pmap <- takeMVar mvar
    putMVar mvar (M.insert (playerId player) player pmap) -- insert player at { playerId: Player }

findPlayer :: DB -> PlayerId -> IO (Maybe Player)
findPlayer (DB mvar) idx = do
    pmap <- takeMVar mvar
    putMVar mvar pmap
    return (M.lookup idx pmap)

--updatePlayer :: DB -> PlayerId -> Player -> IO ()

-- attempt at a thread-safe logger
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

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( start
    ) where

import Api

import Control.Concurrent (forkIO, MVar, newEmptyMVar, takeMVar, putMVar, readMVar, newMVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (runSettings, setPort, setBeforeMainLoop, defaultSettings)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant (serve, (:<|>)(..), Proxy(..), Server, Handler, err404, err500)
import System.IO
import qualified Data.Map as M

start :: IO ()
start = do
    let port = 4000
        settings =
            setPort port $
            setBeforeMainLoop (hPutStrLn stderr
                ("listening on port " `mappend` show port))
            defaultSettings
    runSettings settings =<< app

-- Note that  f <$> x  is the same thing as  do v <- x; return (f v)
-- do { s <- return 1 ; return (s + 1) ; } :: Maybe Int    =====    return 1 >>= \s -> return (s + 1) :: Maybe Int
-- app = simpleCors . serve api <$> getServer
app :: IO Application
app = do
    s <- getServer
    return $ logStdoutDev $ simpleCors $ serve api s

getServer :: IO (Server Api)
getServer = do
    db <- playerDB
    return $ server db

server :: DB -> Server Api
server db =
    getPlayers db :<|>
    getPlayerById db :<|>
    postPlayer db :<|>
    updatePlayerById db

getPlayers :: DB -> Handler [Player]
getPlayers db = liftIO $ getPmap db

getPlayerById :: DB -> PlayerId -> Handler Player
getPlayerById db id = maybe (throwE err404) return =<< liftIO (findPlayer db id)

postPlayer :: DB -> PlayerId -> Player -> Handler Player
postPlayer db id player = maybe (throwE err500) return =<< liftIO (insertPlayer db player >> findPlayer db id)

updatePlayerById :: DB -> PlayerId -> Player -> Handler Player
updatePlayerById db id player = maybe (throwE err500) return =<< liftIO (updatePlayer db id player >> findPlayer db id)
-- (\x -> maybe (throwE err500) (return) x) =<< liftIO (updatePlayer db id player >> findPlayer db id)

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
getPmap (DB mvar) = M.elems <$> readMVar mvar

playerDB :: IO DB
playerDB = DB <$> newMVar (M.fromList existingPlayers)

insertPlayer :: DB -> Player -> IO ()
insertPlayer (DB mvar) player = do
    pmap <- takeMVar mvar
    putMVar mvar (M.insert (playerId player) player pmap) -- insert player at { playerId: Player }

findPlayer :: DB -> PlayerId -> IO (Maybe Player)
findPlayer (DB mvar) idx = do
    pmap <- takeMVar mvar
    putMVar mvar pmap
    return (M.lookup idx pmap)

updatePlayer :: DB -> PlayerId -> Player -> IO ()
updatePlayer (DB mvar) id player = do
    pmap <- takeMVar mvar
    if M.member id pmap
        then putMVar mvar (M.insert id player pmap)
        else putMVar mvar pmap


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

logMessage :: Logger -> String -> IO ()
logMessage (Logger m) s = putMVar m (Log s)

logStop :: Logger -> IO ()
logStop (Logger m) = do
  s <- newEmptyMVar
  putMVar m (End s)
  takeMVar s

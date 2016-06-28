{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module Lib
    ( start
    ) where

import  Control.Monad.Trans.Except
import  Data.Aeson
import  GHC.Generics
import  Network.Wai
import  Network.Wai.Handler.Warp
import  Servant
import  System.IO
import  Api

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
        found = filter (\x -> playerId x == id) examplePlayers
        isFound = not (null found)
    in
        if isFound then return $ head found else throwE err404

existingPlayers :: [Player]
existingPlayers =
    [ Player 1 "Sally" 2
    , Player 2 "Lance" 1
    , Player 3 "Aki" 3
    , Player 4 "Maria" 4
    ]

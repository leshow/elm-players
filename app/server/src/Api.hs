{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Servant.API

type Api =
  "api" :>
    ("player" :> Get '[JSON] [Player] :<|>
     "player" :> Capture "playerId" PlayerId :> Get '[JSON] Player :<|>
     "player" :> Capture "playerId" PlayerId :> ReqBody '[JSON] Player :> Post '[JSON] Player)
     --"player" :> Capture "playerId" PlayerId :> Patch '[JSON] Player)

api :: Proxy Api
api = Proxy

type PlayerId = Integer

data Player
  = Player {
    playerId :: PlayerId,
    playerName :: String,
    playerLevel :: Integer
  }
  deriving (Eq, Show, Generic)

instance ToJSON Player
instance FromJSON Player

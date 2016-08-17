{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Control.Monad.IO.Class
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import DB.SQLite
import Types

type InventoryAPI =
  "hello" :> Get '[JSON] String
  :<|> "tags" :> Get '[JSON] [Tag]

helloEndpoint :: String
helloEndpoint = "hello world!"

tagsEndpoint :: IO [Tag]
tagsEndpoint = getAllTags

server :: Server InventoryAPI
server =
  return helloEndpoint
  :<|> liftIO tagsEndpoint

inventoryAPI :: Proxy InventoryAPI
inventoryAPI = Proxy

app :: Application
app = serve inventoryAPI server

main :: IO ()
main = run 8081 app

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
  :<|> "tags" :> "create" :> ReqBody '[JSON] IDLessTag :> Post '[JSON] Tag

helloEndpoint :: String
helloEndpoint = "hello world!"

tagsEndpoint :: Handler [Tag]
tagsEndpoint = liftIO getAllTags

tagsCreateEndpoint :: IDLessTag -> Handler Tag
tagsCreateEndpoint tag = liftIO . createTag $ tag

server :: Server InventoryAPI
server =
  return helloEndpoint
  :<|> tagsEndpoint
  :<|> tagsCreateEndpoint

inventoryAPI :: Proxy InventoryAPI
inventoryAPI = Proxy

app :: Application
app = serve inventoryAPI server

main :: IO ()
main = run 8081 app

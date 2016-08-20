module Main where

import Control.Monad.IO.Class
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Config
import DB.SQLite
import Types

tagsEndpoint :: Handler [Tag]
tagsEndpoint = liftIO getAllTags

tagsCreateEndpoint :: IDLessTag -> Handler Tag
tagsCreateEndpoint tag = liftIO . createTag $ tag

tagsInfoEndpoint :: Integer -> Handler (Maybe Tag)
tagsInfoEndpoint tid = liftIO . selectTagById $ tid

server :: Server InventoryAPI
server =
  tagsEndpoint
  :<|> tagsCreateEndpoint
  :<|> tagsInfoEndpoint

app :: Application
app = serve inventoryAPI server

main :: IO ()
main = run (fromIntegral serverPort) app

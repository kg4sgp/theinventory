{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as LB
import Control.Monad.IO.Class
import Network.Wai
import Network.Wai.Handler.Warp
import Servant

import Config
import DB.SQLite
import Types

-- Tags

tagsEndpoint :: Handler [Tag]
tagsEndpoint = liftIO getAllTags

-- TODO: We need to make sure the parent tag is valid!!
tagsCreateEndpoint :: IDLessTag -> Handler Tag
tagsCreateEndpoint tag = liftIO . createTag $ tag

tagsInfoEndpoint :: Integer -> Handler Tag
tagsInfoEndpoint tid =
  liftIO (selectTagById tid) >>= flip getOr404 "No such tag"

tagsItemsEndpoint :: Integer -> Handler [Item]
tagsItemsEndpoint tid = liftIO (selectItemsByTagId tid)

-- Items

itemsEndpoint :: Handler [Item]
itemsEndpoint = liftIO getAllItems

itemsCreateEndpoint :: IDLessItem -> Handler Item
itemsCreateEndpoint tag = liftIO . createItem $ tag

itemsInfoEndpoint :: Integer -> Handler Item
itemsInfoEndpoint iid =
  liftIO (selectItemById iid) >>= flip getOr404 "No such item"

server :: Server InventoryAPI
server =
  tagsEndpoint
  :<|> tagsCreateEndpoint
  :<|> tagsInfoEndpoint
  :<|> tagsItemsEndpoint
  :<|> itemsEndpoint
  :<|> itemsCreateEndpoint
  :<|> itemsInfoEndpoint

app :: Application
app = serve inventoryAPI server

main :: IO ()
main = run (fromIntegral serverPort) app

getOr404 :: Maybe a -> LB.ByteString -> Handler a
getOr404 (Just a) _ = return a
getOr404 Nothing s = throwError (err404 { errBody = s })

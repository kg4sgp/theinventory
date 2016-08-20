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

tagsEndpoint :: Handler [Tag]
tagsEndpoint = liftIO getAllTags

tagsCreateEndpoint :: IDLessTag -> Handler Tag
tagsCreateEndpoint tag = liftIO . createTag $ tag

tagsInfoEndpoint :: Integer -> Handler Tag
tagsInfoEndpoint tid =
  liftIO (selectTagById tid) >>= flip getOr404 "No such tag"

server :: Server InventoryAPI
server =
  tagsEndpoint
  :<|> tagsCreateEndpoint
  :<|> tagsInfoEndpoint

app :: Application
app = serve inventoryAPI server

main :: IO ()
main = run (fromIntegral serverPort) app

getOr404 :: Maybe a -> LB.ByteString -> Handler a
getOr404 (Just a) _ = return a
getOr404 Nothing s = throwError (err404 { errBody = s })

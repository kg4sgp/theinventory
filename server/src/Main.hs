{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type InventoryAPI =
  "hello" :> Get '[JSON] String

server :: Server InventoryAPI
server = return "hello world!"

inventoryAPI :: Proxy InventoryAPI
inventoryAPI = Proxy

app :: Application
app = serve inventoryAPI server

main :: IO ()
main = run 8081 app

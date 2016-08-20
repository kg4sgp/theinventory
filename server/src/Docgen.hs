{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Servant
import Servant.Docs
import Types

instance ToSample IDLessTag where
  toSamples _ =
    [ ("When there is no parent tag", IDLessTag "communication" Nothing)
    , ("When specifying a parent tag", IDLessTag "cellphone" (Just 1))
    ]

instance ToSample Tag where
  toSamples _ =
    [ ("When there is no parent tag", Tag 1 "communication" Nothing)
    , ("When specifying a parent tag", Tag 2 "cellphone" (Just 1))
    ]

instance ToCapture (Capture "tag_id" Integer) where
  toCapture _ =
    DocCapture "tag_id"
               "(integer) the id number of the tag"

apiDocs :: API
apiDocs = docs inventoryAPI

main :: IO ()
main = writeFile "API.md" (markdown apiDocs)

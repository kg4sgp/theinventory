{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Time
import Servant
import Servant.Docs
import Types

fakeTime :: UTCTime
fakeTime = parseTimeOrError True defaultTimeLocale "%s" "1472771084"

instance ToSample IDLessTag where
  toSamples _ =
    [ ("When there is no parent tag", IDLessTag "communication" Nothing)
    , ("When specifying a parent tag", IDLessTag "cellphone" (Just 1))
    ]

instance ToSample Tag where
  toSamples _ =
    [ ("When there is no parent tag", Tag 1 "communication" fakeTime Nothing)
    , ("When specifying a parent tag", Tag 2 "cellphone" fakeTime (Just 1))
    ]

instance ToSample Item where
  toSamples _ =
    [ ("An item", Item 1 "iPhone 4" 1.0 "050145151651560" fakeTime)
    ]

instance ToSample IDLessItem where
  toSamples _ =
    [ ("An item", IDLessItem "iPhone 4" 1.0 "050145151651560")
    ]

instance ToCapture (Capture "tag_id" Integer) where
  toCapture _ =
    DocCapture "tag_id"
               "(integer) the id number of the tag"

instance ToCapture (Capture "item_id" Integer) where
  toCapture _ =
    DocCapture "item_id"
               "(integer) the id number of the item (not the barcode)"

apiDocs :: API
apiDocs = docs inventoryAPI

main :: IO ()
main = writeFile "API.md" (markdown apiDocs)

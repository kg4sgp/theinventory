{-# LANGUAGE OverloadedStrings #-}
module DB.SQLite where

import qualified Config
import Database.SQLite.Simple
import Types

-- Tag

instance FromRow Tag where
  fromRow = Tag <$> field <*> field <*> field

instance ToRow Tag where
  toRow (Tag tagId' tagName' tagParentTag') =
    toRow (tagId', tagName', tagParentTag')


-- Row

instance FromRow Item where
  fromRow = Item <$> field <*> field <*> field <*> field

instance ToRow Item where
  toRow (Item itemId' itemName' itemRating' itemBarcode') =
    toRow (itemId', itemName', itemRating', itemBarcode')

-- Queries
-- For efficiency, we could/should pass around the 'Connection' in a Reader
-- monad.

getAllTags :: IO [Tag]
getAllTags = do
  conn <- open Config.sqliteDatabasePath
  tags <- query_ conn "select * from tags;" :: IO [Tag]
  return tags

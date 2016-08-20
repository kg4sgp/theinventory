{-# LANGUAGE OverloadedStrings #-}
module DB.SQLite where

import qualified Config
import Data.Maybe (listToMaybe)
import Database.SQLite.Simple
import Types

-- Tag

instance FromRow Tag where
  fromRow = Tag <$> field <*> field <*> field

instance ToRow Tag where
  toRow (Tag tagId' tagName' tagParentTag') =
    toRow (tagId', tagName', tagParentTag')


-- Item

instance FromRow Item where
  fromRow = Item <$> field <*> field <*> field <*> field

instance ToRow Item where
  toRow (Item itemId' itemName' itemRating' itemBarcode') =
    toRow (itemId', itemName', itemRating', itemBarcode')

-- IDLessTag

instance FromRow IDLessTag where
  fromRow = IDLessTag <$> field <*> field

instance ToRow IDLessTag where
  toRow (IDLessTag idLessTagName' idLessTagParentTag') =
    toRow (idLessTagName', idLessTagParentTag')


-- Queries
-- For efficiency, we could/should pass around the 'Connection' in a Reader
-- monad.

getAllTags :: IO [Tag]
getAllTags = do
  conn <- open Config.sqliteDatabasePath
  tags <- query_ conn "select * from tags;" :: IO [Tag]
  return tags

selectTagById :: Integer -> IO (Maybe Tag)
selectTagById tid = do
  conn <- open Config.sqliteDatabasePath
  listToMaybe <$> query conn "select * from tags where id=?;" (Only tid)

createTag :: IDLessTag -> IO Tag
createTag tag = do
  conn <- open Config.sqliteDatabasePath
  execute conn "insert into tags (name, parent_tag) values (?, ?);" tag
  rowId <- lastInsertRowId conn
  -- TODO: Is it safe to rely on this? It saves a SELECT on the new row
  -- but it assumes that the insertion was successful always.
  return $ idLessTagToTag tag (fromIntegral rowId)

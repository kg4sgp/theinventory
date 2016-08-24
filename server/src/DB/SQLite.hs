{-# LANGUAGE OverloadedStrings #-}
module DB.SQLite where

import qualified Config
import Data.Maybe (listToMaybe)
import Database.SQLite.Simple
import Types

-- Tag

instance FromRow Tag where
  fromRow = Tag <$> field <*> field <*> field <*> field

instance ToRow Tag where
  toRow (Tag tagId' tagName' tagCreationDt' tagParentTag') =
    toRow (tagId', tagName', tagCreationDt', tagParentTag')

-- IDLessTag

instance ToRow IDLessTag where
  toRow (IDLessTag idLessTagName' idLessTagParentTag') =
    toRow (idLessTagName', idLessTagParentTag')

-- Item

instance FromRow Item where
  fromRow = Item <$> field <*> field <*> field <*> field <*> field

instance ToRow Item where
  toRow (Item itemId' itemName' itemRating' itemBarcode' itemCreationDt') =
    toRow (itemId', itemName', itemRating', itemBarcode', itemCreationDt')

-- IDLessItem

instance ToRow IDLessItem where
  toRow (IDLessItem name rating barcode) = toRow (name, rating, barcode)

-- TagItem

instance FromRow TagItem where
  fromRow = TagItem <$> field <*> field <*> field <*> field

instance ToRow TagItem where
  toRow (TagItem tiid titid tiiid ticdt) = toRow (tiid, titid, tiiid, ticdt)

-- IDLessTagItem

instance ToRow IDLessTagItem where
  toRow (IDLessTagItem titid tiiid) = toRow (titid, tiiid)


-- Queries
-- For efficiency, we could/should pass around the 'Connection' in a Reader
-- monad.

-- Tags

getAllTags :: IO [Tag]
getAllTags = do
  conn <- open Config.sqliteDatabasePath
  query_ conn "select * from tags;" :: IO [Tag]

selectTagById :: Integer -> IO (Maybe Tag)
selectTagById tid = do
  conn <- open Config.sqliteDatabasePath
  listToMaybe <$> query conn "select * from tags where id=?;" (Only tid)

createTag :: IDLessTag -> IO (Maybe Tag)
createTag tag = do
  conn <- open Config.sqliteDatabasePath
  execute conn "insert into tags (name, parent_tag) values (?, ?);" tag
  rowId <- lastInsertRowId conn
  selectTagById (fromIntegral rowId)

-- Items

getAllItems :: IO [Item]
getAllItems = do
  conn <- open Config.sqliteDatabasePath
  query_ conn "select * from items;" :: IO [Item]

selectItemById :: Integer -> IO (Maybe Item)
selectItemById iid = do
  conn <- open Config.sqliteDatabasePath
  listToMaybe <$> query conn "select * from items where id=?;" (Only iid)

createItem :: IDLessItem -> IO (Maybe Item)
createItem itm = do
  conn <- open Config.sqliteDatabasePath
  execute conn "insert into items (name, rating, barcode) values (?, ?, ?);" itm
  rowId <- lastInsertRowId conn
  selectItemById (fromIntegral rowId)

-- TagItems

getAllTagItems :: IO [TagItem]
getAllTagItems = do
  conn <- open Config.sqliteDatabasePath
  query_ conn "select * from tags_items;" :: IO [TagItem]

selectTagItemById :: Integer -> IO (Maybe TagItem)
selectTagItemById tiid = do
  conn <- open Config.sqliteDatabasePath
  listToMaybe <$> query conn "select * from tags_items where id=?;" (Only tiid)

createTagItem :: IDLessTagItem -> IO (Maybe TagItem)
createTagItem titm = do
  conn <- open Config.sqliteDatabasePath
  execute conn "insert into tags_items (tag_id, item_id) values (?, ?);" titm
  rowId <- lastInsertRowId conn
  selectTagItemById (fromIntegral rowId)

selectItemsByTagId :: Integer -> IO [Item]
selectItemsByTagId tid = do
  conn <- open Config.sqliteDatabasePath
  -- TODO: When we move off sqlite, see if a JOIN is better performing than this
  -- subquery.
  query conn "select * from items where id in (select item_id from tags_items where tag_id=?);" (Only tid)

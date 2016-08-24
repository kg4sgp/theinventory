{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Types where

import Data.Aeson
import Data.Monoid (mempty)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Servant

-- | The main API type.
type InventoryAPI =
  "tags" :> Get '[JSON] [Tag]
  :<|> "tags" :> "create" :> ReqBody '[JSON] IDLessTag :> Post '[JSON] (Maybe Tag)
  :<|> "tags" :> "info" :> Capture "tag_id" Integer :> Get '[JSON] Tag
  :<|> "tags" :> "items" :> Capture "tag_id" Integer :> Get '[JSON] [Item]
  :<|> "items" :> Get '[JSON] [Item]
  :<|> "items" :> "create" :> ReqBody '[JSON] IDLessItem :> Post '[JSON] (Maybe Item)
  :<|> "items" :> "info" :> Capture "item_id" Integer :> Get '[JSON] Item

inventoryAPI :: Proxy InventoryAPI
inventoryAPI = Proxy

-- | A 'Tag' *after* it is added to the database (and thus has an id).
data Tag =
  Tag { tagId :: Integer
      , tagName :: T.Text
      , tagCreationDt :: UTCTime
      , tagParentTag :: Maybe Integer
      } deriving (Eq, Ord, Show)

instance FromJSON Tag where
  parseJSON (Object v) = Tag <$>
                         v .: "id" <*>
                         v .: "name" <*>
                         v .: "creation_time" <*>
                         v .: "parent_tag"
  parseJSON _          = mempty

instance ToJSON Tag where
  toJSON (Tag tagId' tagName' tagCreationDt' tagParentTag') =
    object [ "id" .= tagId'
           , "name" .= tagName'
           , "creation_time" .= tagCreationDt'
           , "parent_tag" .= tagParentTag'
           ]

-- | A 'Tag' without an id number associated with it.
--
-- If we get a tag from the database, it's guaranteed to have an id number, so
-- it makes sense to note that in the 'Tag' constructor above (i.e., rather than
-- making 'tagId' be 'Maybe' 'Integer'). So instead we create a type for a 'Tag'
-- that doesn\'t have an id number. This is what gets instantiated by the API
-- and ultimately passed to 'createTag'.
data IDLessTag =
  IDLessTag { idLessTagName :: T.Text
            , idLessTagParentTag :: Maybe Integer
            } deriving (Eq, Ord, Show)

instance FromJSON IDLessTag where
  parseJSON (Object v) = IDLessTag <$>
                         v .: "name" <*>
                         v .: "parent_tag"
  parseJSON _          = mempty

instance ToJSON IDLessTag where
  toJSON (IDLessTag tagName' tagParentTag') =
    object ["name" .= tagName', "parent_tag" .= tagParentTag']

-- | An injection from 'IDLessTag' to 'Tag'.
idLessTagToTag :: IDLessTag -> Integer -> UTCTime -> Tag
idLessTagToTag (IDLessTag idLessTagName' idLessTagParentTag') tid time =
  Tag tid idLessTagName' time idLessTagParentTag'

-- | An 'Item' *after* it is added to the datrabase (and thus has an id).
data Item =
  Item { itemId :: Integer
       , itemName :: T.Text
       , itemRating :: Float
       , itemBarcode :: T.Text
       , itemCreationDt :: UTCTime
       } deriving (Eq, Ord, Show)

instance FromJSON Item where
  parseJSON (Object v) = Item <$>
                         v .: "id" <*>
                         v .: "name" <*>
                         v .: "rating" <*>
                         v .: "barcode" <*>
                         v .: "creation_time"
  parseJSON _          = mempty

instance ToJSON Item where
  toJSON (Item itemId' itemName' itemRating' itemBarcode' itemCreationDt') =
    object [ "id" .= itemId'
           , "name" .= itemName'
           , "rating" .= itemRating'
           , "barcode" .= itemBarcode'
           , "creation_time" .= itemCreationDt'
           ]

-- | An 'Item' *before* it is added to the datrabase (and thus has no id).
data IDLessItem =
  IDLessItem { idLessItemName :: T.Text
             , idLessItemRating :: Float
             , idLessItemBarcode :: T.Text
             } deriving (Eq, Ord, Show)

instance FromJSON IDLessItem where
  parseJSON (Object v) = IDLessItem <$>
                         v .: "name" <*>
                         v .: "rating" <*>
                         v .: "barcode"
  parseJSON _          = mempty

instance ToJSON IDLessItem where
  toJSON (IDLessItem name rating barcode) =
    object [ "name" .= name
           , "rating" .= rating
           , "barcode" .= barcode
           ]

-- | An injection from 'IDLessItem' to 'Item'.
idLessItemToItem :: IDLessItem -> Integer -> UTCTime -> Item
idLessItemToItem (IDLessItem name rating barcode) iid time =
  Item iid name rating barcode time

-- | An 'TagItem' *after* it is added to the datrabase (and thus has an id).
--
-- This is an association between an 'Item' and a 'Tag'.
data TagItem =
  TagItem { tagItemId :: Integer
          , tagItemTagId :: Integer
          , tagItemItemId :: Integer
          , tagItemCreationDt :: UTCTime
          } deriving (Eq, Ord, Show)

instance FromJSON TagItem where
  parseJSON (Object v) = TagItem <$>
                         v .: "id" <*>
                         v .: "tag_id" <*>
                         v .: "item_id" <*>
                         v .: "creation_dt"
  parseJSON _          = mempty

instance ToJSON TagItem where
  toJSON (TagItem tiid titid tiiid ticdt) =
    object [ "id" .= tiid
           , "tag_id" .= titid
           , "item_id" .= tiiid
           , "creation_dt" .= ticdt
           ]

-- | An 'TagItem' *before* it is added to the datrabase (and thus has an id).
--
-- This is an association between an 'Item' and a 'Tag'.
data IDLessTagItem =
  IDLessTagItem { idLessTagItemTagId :: Integer
                , idLessTagItemItemId :: Integer
                } deriving (Eq, Ord, Show)

instance FromJSON IDLessTagItem where
  parseJSON (Object v) = IDLessTagItem <$>
                         v .: "tag_id" <*>
                         v .: "item_id"
  parseJSON _          = mempty

instance ToJSON IDLessTagItem where
  toJSON (IDLessTagItem titid tiiid) =
    object [ "tag_id" .= titid
           , "item_id" .= tiiid
           ]

-- | An injection from 'IDTagItem' to 'TagItem'.
idLessTagItemToTagItem :: IDLessTagItem -> Integer -> UTCTime -> TagItem
idLessTagItemToTagItem (IDLessTagItem titid tiiid) tiid time =
  TagItem tiid titid tiiid time

{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Aeson
import Data.Monoid (mempty)
import qualified Data.Text as T

data Tag =
  Tag { tagId :: Integer
      , tagName :: T.Text
      , tagParentTag :: Maybe Integer
      } deriving (Eq, Ord, Show)

instance FromJSON Tag where
  parseJSON (Object v) = Tag <$>
                         v .: "id" <*>
                         v .: "name" <*>
                         v .: "parent_tag"
  parseJSON _          = mempty

instance ToJSON Tag where
  toJSON (Tag tagId' tagName' tagParentTag') =
    object ["id" .= tagId', "name" .= tagName', "parent_tag" .= tagParentTag']

data Item =
  Item { itemId :: Integer
       , itemName :: T.Text
       , itemRating :: Float
       , itemBarcode :: T.Text
       } deriving (Eq, Ord, Show)

instance FromJSON Item where
  parseJSON (Object v) = Item <$>
                         v .: "id" <*>
                         v .: "name" <*>
                         v .: "rating" <*>
                         v .: "barcode"
  parseJSON _          = mempty

instance ToJSON Item where
  toJSON (Item itemId' itemName' itemRating' itemBarcode') =
    object [ "id" .= itemId'
           , "name" .= itemName'
           , "rating" .= itemRating'
           , "barcode" .= itemBarcode'
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
idLessTagToTag :: IDLessTag -> Integer -> Tag
idLessTagToTag (IDLessTag idLessTagName' idLessTagParentTag') tid =
  Tag tid idLessTagName' idLessTagParentTag'

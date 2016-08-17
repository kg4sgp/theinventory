{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Aeson
import Data.Monoid (mempty)
import qualified Data.Text as T

data Tag =
  Tag { tagId :: Integer
      , tagName :: T.Text
      } deriving (Eq, Ord, Show)

instance FromJSON Tag where
  parseJSON (Object v) = Tag <$>
                         v .: "id" <*>
                         v .: "name"
  parseJSON _          = mempty

instance ToJSON Tag where
  toJSON (Tag tagId' tagName') =
    object ["id" .= tagId', "name" .= tagName']

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

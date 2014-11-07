{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module JSON.EventGroup.Request
( Meta(..)
, Data(..)
, Message(..)
, encode
, decode
, eitherDecode
) where

import GHC.Generics
import Data.Text
import Data.Aeson

data Meta = Meta
  { h_type :: Text
  , h_id :: [Char]
  , h_group :: Text
  } deriving (Show, Generic)

data Data = Data
  { h_eventGroupType :: Text
  , h_eventGroupId :: [Char]
  , h_ownerType :: Text
  , h_ownerId :: [Char]
  } deriving (Show, Generic)

data Message = Message
  { h_meta :: Meta
  , h_data :: Data
  } deriving (Show, Generic)

instance FromJSON Meta
instance FromJSON Data
instance FromJSON Message

instance ToJSON Meta
instance ToJSON Data
instance ToJSON Message

{-# LANGUAGE OverloadedStrings, DeriveGeneric, MultiParamTypeClasses #-}

module Main where

import HelpEsbClient
import Database.PostgreSQL.Simple

-- JSON Data Structures
import qualified JSON.Event.Response as Event.Response
import qualified JSON.EventGroup.Response as EventGroup.Response
import qualified JSON.API.EventGroup.Post.Request as EventGroup.Post.Request
import qualified JSON.API.Event.Post.Request as Event.Post.Request

-- Recieve Socket Instances
instance EsbRecieveExternal EventGroup.Response.Message Connection where
  esbRecieveExternal sock message db = do
    let payload = EventGroup.Response.h_data message
    logger ("EventGroup Response: " ++ show payload)
    -- Hit the API
    let request = EventGroup.Post.Request.Data {
        EventGroup.Post.Request.h_eventGroupType = EventGroup.Response.h_eventGroupType payload
      , EventGroup.Post.Request.h_eventGroupId = EventGroup.Response.h_eventGroupId payload
      , EventGroup.Post.Request.h_ownerType = EventGroup.Response.h_ownerType payload
      , EventGroup.Post.Request.h_ownerId = EventGroup.Response.h_ownerId payload
      }
    esbSend sock request

instance EsbRecieveExternal Event.Response.Message Connection where
  esbRecieveExternal sock message db = do
    let payload = Event.Response.h_data message
    logger ("Event Response: " ++ show payload)
    -- Hit the API
    let request = Event.Post.Request.Data {
        Event.Post.Request.h_createdAt = Event.Response.h_createdAt payload
      , Event.Post.Request.h_content = Event.Response.h_content payload
      , Event.Post.Request.h_eventType = Event.Response.h_eventType payload
      , Event.Post.Request.h_senderType = Event.Response.h_senderType payload
      , Event.Post.Request.h_senderId = Event.Response.h_senderId payload
      , Event.Post.Request.h_customerId = Event.Response.h_customerId payload
      }
    esbSend sock request


-- ESB Environment
host = Nothing
port = Nothing

-- Database Environment
dbHost = "api.help.com"
dbName = "help"

-- Listening Recursion
listen :: Socket -> Connection -> IO ()
listen sock db = do
  -- Perform essential listening logic
  bytes <- esbListen sock

  case eitherDecode bytes :: (Either String EventGroup.Response.Message) of
    Left error -> return ()
    Right response -> do
      logger ("Response: " ++ show response)
      esbRecieveExternal sock response db

  case eitherDecode bytes :: (Either String Event.Response.Message) of
    Left error -> return ()
    Right response -> do
      logger ("Response: " ++ show response)
      esbRecieveExternal sock response db

  -- Recurse
  listen sock db

-- Initialization
main :: IO ()
main = do
  -- Connect to database
  db <- connect defaultConnectInfo {
      connectHost = dbHost
    , connectDatabase = dbName
    , connectUser = "postgres"
    , connectPassword = "abc123"
    }

  -- Connect to socket and login
  sock <- esbInit "event-muncher" [ "event-messages" ] host port

  -- Start Listening
  listen sock db

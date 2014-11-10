{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import HelpEsbClient

-- JSON Data Structures
import qualified JSON.Event.Response as Event.Response
import qualified JSON.EventGroup.Response as EventGroup.Response
import qualified JSON.API.EventGroup.Post.Request as EventGroup.Post.Request
import qualified JSON.API.Event.Post.Request as Event.Post.Request

-- Recieve Socket Instances
instance EsbRecieve EventGroup.Response.Message where
  esbRecieve sock message = do
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

instance EsbRecieve Event.Response.Message where
  esbRecieve sock message = do
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
host = "127.0.0.1"
port = 8900

-- Listening Recursion
listen :: Socket -> IO ()
listen sock = do
  -- Perform essential listening logic
  bytes <- esbListen sock

  case eitherDecode bytes :: (Either String EventGroup.Response.Message) of
    Left error -> return ()
    Right response -> do
      logger ("Response: " ++ show response)
      esbRecieve sock response

  case eitherDecode bytes :: (Either String Event.Response.Message) of
    Left error -> return ()
    Right response -> do
      logger ("Response: " ++ show response)
      esbRecieve sock response

  -- Recurse
  listen sock

-- Initialization
main :: IO ()
main = do
  -- Connect to socket and login
  sock <- esbInit "event-muncher" [ "event-messages" ] host port

  -- Start Listening
  listen sock

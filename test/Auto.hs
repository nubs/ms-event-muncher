{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import HelpEsbClient

-- JSON Data Structures
import qualified JSON.Event.Request as Event.Request
import qualified JSON.Event.Response as Event.Response
import qualified JSON.EventGroup.Request as EventGroup.Request
import qualified JSON.EventGroup.Response as EventGroup.Response

-- Send Socket Instances
instance EsbSend EventGroup.Request.Data where
  esbSend sock payload = do
    uuid <- nextRandom
    let meta = EventGroup.Request.Meta {
          EventGroup.Request.h_type = "sendMessage"
        , EventGroup.Request.h_id = toString uuid
        , EventGroup.Request.h_group = "event-messages"
        }
    let message = EventGroup.Request.Message {
          EventGroup.Request.h_meta = meta
        , EventGroup.Request.h_data = payload
        }
    sendSocketData sock (encode message)

instance EsbSend Event.Request.Data where
  esbSend sock payload = do
    uuid <- nextRandom
    let meta = Event.Request.Meta {
          Event.Request.h_type = "sendMessage"
        , Event.Request.h_id = toString uuid
        , Event.Request.h_group = "event-messages"
        }
    let message = Event.Request.Message {
          Event.Request.h_meta = meta
        , Event.Request.h_data = payload
        }
    sendSocketData sock (encode message)

-- Recieve Socket Instances
instance EsbRecieve EventGroup.Response.Message where
  esbRecieve sock message = do
    let payload = EventGroup.Response.h_data message
    logger("EventGroup Response: " ++ show payload)

instance EsbRecieve Event.Response.Message where
  esbRecieve sock message = do
    let payload = Event.Response.h_data message
    logger("Event Response: " ++ show payload)

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
      logger("Response: " ++ show response)
      esbRecieve sock response


  case eitherDecode bytes :: (Either String Event.Response.Message) of
    Left error -> return ()
    Right response -> do
      logger("Response: " ++ show response)
      esbRecieve sock response

  -- Recurse
  listen sock

-- Initialization
main :: IO ()
main = do
  -- Connect to socket and login
  sock <- esbInit "event-muncher-test-auto" [ "event-messages" ] host port

  egIdUno <- nextRandom
  oIdUno <- nextRandom
  let egUno = EventGroup.Request.Data {
      EventGroup.Request.h_eventGroupType = "chat"
    , EventGroup.Request.h_eventGroupId = toString egIdUno
    , EventGroup.Request.h_ownerType = "user"
    , EventGroup.Request.h_ownerId = toString oIdUno
    }
  esbSend sock egUno

  sIdUno <- nextRandom
  cIdUno <- nextRandom
  let eUno = Event.Request.Data {
      Event.Request.h_createdAt = 1000
    , Event.Request.h_content = "I need some shoes."
    , Event.Request.h_eventType = "message"
    , Event.Request.h_senderType = "customer"
    , Event.Request.h_senderId = toString sIdUno
    , Event.Request.h_customerId = toString cIdUno
    , Event.Request.h_eventGroupId = toString egIdUno
    }
  esbSend sock eUno

  sIdDos <- nextRandom
  cIdDos <- nextRandom
  let eDos = Event.Request.Data {
      Event.Request.h_createdAt = 1000
    , Event.Request.h_content = "How do I do a barrel roll?"
    , Event.Request.h_eventType = "message"
    , Event.Request.h_senderType = "customer"
    , Event.Request.h_senderId = toString sIdUno
    , Event.Request.h_customerId = toString cIdUno
    , Event.Request.h_eventGroupId = toString egIdUno
    }
  esbSend sock eDos

  egIdDos <- nextRandom
  oIdDos <- nextRandom
  let egDos = EventGroup.Request.Data {
      EventGroup.Request.h_eventGroupType = "session"
    , EventGroup.Request.h_eventGroupId = toString egIdDos
    , EventGroup.Request.h_ownerType = "user"
    , EventGroup.Request.h_ownerId = toString oIdDos
    }
  esbSend sock egDos

  sIdTres <- nextRandom
  cIdTres <- nextRandom
  let eTres = Event.Request.Data {
      Event.Request.h_createdAt = 1000
    , Event.Request.h_content = "I need some shoes."
    , Event.Request.h_eventType = "message"
    , Event.Request.h_senderType = "customer"
    , Event.Request.h_senderId = toString sIdTres
    , Event.Request.h_customerId = toString cIdTres
    , Event.Request.h_eventGroupId = toString egIdDos
    }
  esbSend sock eTres

  -- Start Listening
  listen sock

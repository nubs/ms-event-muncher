{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main where

import HelpEsbClient

-- JSON Data Structures
import qualified JSON.Event.Response as Event.Response
import qualified JSON.EventGroup.Response as EventGroup.Response

-- Recieve Socket Instances
instance EsbRecieve EventGroup.Response.Message where
  esbRecieve sock message = do
    let payload = EventGroup.Response.h_data message
    logger("EventGroup Response: " ++ show payload)
    -- Eventually hit API once the microservice is finished.

instance EsbRecieve Event.Response.Message where
  esbRecieve sock message = do
    let payload = Event.Response.h_data message
    logger("Event Response: " ++ show payload)
    -- Eventually hit API once the microservice is finished.

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
  sock <- esbInit "event-muncher" [ "event-messages" ] host port

  -- Start Listening
  listen sock

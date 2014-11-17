{-# LANGUAGE OverloadedStrings, DeriveGeneric, MultiParamTypeClasses #-}

module Main where

import HelpEsbClient
import Database.PostgreSQL.Simple
import System.Locale
import Data.List.Split
import Data.Map
import Data.Maybe
import Data.Time
import Data.DateTime

-- JSON Data Structures
import qualified JSON.Event.Response as Event.Response
import qualified JSON.EventGroup.Response as EventGroup.Response

-- Recieve Socket Instances.
instance EsbRecieveExternal EventGroup.Response.Message Connection where
  esbRecieveExternal sock message db = do
    let payload = EventGroup.Response.h_data message
    logger ("EventGroup Response: " ++ show payload)

    let eventGroupType = EventGroup.Response.h_eventGroupType payload
    let eventGroupId = EventGroup.Response.h_eventGroupId payload
    let ownerType = EventGroup.Response.h_ownerType payload
    let ownerId = EventGroup.Response.h_ownerId payload

    insert <- execute db "INSERT INTO event_group (id, type, owner_type, owner_id) VALUES (?, ?, ?, ?)" (eventGroupId, eventGroupType, ownerType, ownerId)
    return ()

instance EsbRecieveExternal Event.Response.Message Connection where
  esbRecieveExternal sock message db = do
    let payload = Event.Response.h_data message
    logger ("Event Response: " ++ show payload)

    eventId <- nextRandom
    let createdAtUtc = fromSeconds (Event.Response.h_createdAt payload)
    let content = Event.Response.h_content payload
    let eventType = Event.Response.h_eventType payload
    let senderType = Event.Response.h_senderType payload
    let senderId = Event.Response.h_senderId payload
    let customerId = Event.Response.h_customerId payload
    let eventGroupId = Event.Response.h_eventGroupId payload

    let createdAt = formatTime defaultTimeLocale "%m/%d/%Y %l:%M:%S" createdAtUtc

    insert <- execute db "INSERT INTO event (id, created_at, content, type, sender_type, sender_id, customer_id, event_group_id) VALUES (?, ?, ?, ?, ?, ?, ?, ?)" ((show eventId), createdAt, content, eventType, senderType, senderId, customerId, eventGroupId)
    return ()

-- ESB Environment
host :: Maybe String
host = Nothing
port :: Maybe Int
port = Nothing

-- Database Environment
dbHost :: Maybe String
dbHost = Nothing
dbPort :: Maybe Int
dbPort = Nothing
dbName :: Maybe String
dbName = Nothing
dbUser :: Maybe String
dbUser = Nothing
dbPassword :: Maybe String
dbPassword = Nothing

-- Listening Recursion
listen :: Socket -> Connection -> IO ()
listen sock db = do
  -- Get messages and perform essential listening logic.
  messages <- esbListen sock

  -- Iterate over messages.
  forM_ messages $ \message -> do
    case eitherDecode message :: (Either String EventGroup.Response.Message) of
      Left error -> return ()
      Right response -> do
        logger ("Response: " ++ show response)
        esbRecieveExternal sock response db

    case eitherDecode message :: (Either String Event.Response.Message) of
      Left error -> return ()
      Right response -> do
        logger ("Response: " ++ show response)
        esbRecieveExternal sock response db

  -- Recurse.
  listen sock db

-- Initialization
main :: IO ()
main = do
  envVars <- getEnvironment
  let envMap = Data.Map.fromList envVars
  let envUriString = fromMaybe "http://postgres@127.0.0.1:8900" (Data.Map.lookup "CACHE" envMap)

  let envUri = fromMaybe nullURI (parseURI envUriString)

  let defaultUriAuth = URIAuth {
      uriUserInfo = "postgres@"
    , uriRegName = "127.0.0.1"
    , uriPort = ":5432"
  }
  let envUriAuth = fromMaybe defaultUriAuth (uriAuthority envUri)
  let userInfo = splitOn ":" (uriUserInfo envUriAuth)

  let envDbHost = uriRegName envUriAuth
  let envDbPort = read (tail (uriPort envUriAuth))
  let envDbName = fromMaybe "default" (Data.Map.lookup "CACHE_DB" envMap)
  let envDbUser = head userInfo
  let envDbPassword = init (userInfo !! 1)

  let dbHost' = fromMaybe envDbHost dbHost
  let dbPort' = fromMaybe envDbPort dbPort
  let dbName' = fromMaybe envDbName dbName
  let dbUser' = fromMaybe envDbUser dbUser
  let dbPassword' = fromMaybe envDbPassword dbPassword

  -- Connect to database.
  db <- connect defaultConnectInfo {
      connectHost = dbHost'
    , connectPort = 5432
    , connectDatabase = dbName'
    , connectUser = dbUser'
    , connectPassword = dbPassword'
    }

  -- Connect to socket and login.
  sock <- esbInit "event-muncher" [ "event-messages" ] host port

  -- Start Listening.
  listen sock db

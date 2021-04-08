{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- |
-- Module: Network.Wai.Session.Redis
-- Copyright: (c) 2021, t4ccer
-- License: BSD3
-- Stability: experimental
-- Portability: portable
--
-- Simple Redis backed wai-session backend. This module allows you to store
-- session data of wai-sessions in a Redis database.
module Network.Wai.Session.Redis
  ( dbStore
  , clearSession
  , SessionSettings(..)
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import           Data.Default
import           Data.Either
import           Data.Serialize         (Serialize, decode, encode)
import           Database.Redis         hiding (decode)
import           Network.Wai.Session

-- | Settings to control session store
data SessionSettings = SessionSettings
  { redisConnectionInfo :: ConnectInfo
  , expiratinTime       :: Integer
  -- ^ Session expiration time in seconds
  }

instance Default SessionSettings where
  def = SessionSettings
    { redisConnectionInfo = defaultConnectInfo
    , expiratinTime       = 60*60*24*7 -- One week
    }

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right a) = Just a

connectAndRunRedis :: MonadIO m => ConnectInfo -> Redis b -> m b
connectAndRunRedis ci cmd = liftIO do
  conn <- connect ci
  res  <- runRedis conn cmd
  disconnect conn
  return res

createSession :: MonadIO m => SessionSettings -> m ByteString
createSession SessionSettings{..} = liftIO do
  sesId <- genSessionId
  connectAndRunRedis redisConnectionInfo $ do
    hset sesId "" ""
    expire sesId expiratinTime
  return sesId

isSesIdValid :: MonadIO m => SessionSettings -> ByteString -> m Bool
isSesIdValid SessionSettings{..} sesId = liftIO do
  res <- connectAndRunRedis redisConnectionInfo $ do
    exists sesId
  return $ fromRight False res

insertIntoSession :: MonadIO m => SessionSettings
  -> ByteString -- ^ Sessionn id
  -> ByteString -- ^ Key
  -> ByteString -- ^ Value
  -> m ()
insertIntoSession SessionSettings{..} sesId key value = do
  connectAndRunRedis redisConnectionInfo $ do
    hset sesId key value
    expire sesId expiratinTime
  return ()

lookupFromSession :: MonadIO m => SessionSettings
  -> ByteString -- ^ Session id
  -> ByteString -- ^ Key
  -> m (Maybe ByteString)
lookupFromSession SessionSettings{..} sesId key = do
  v <- connectAndRunRedis redisConnectionInfo $ do
    v <- hget sesId key
    expire sesId expiratinTime
    return v
  return $ join $ eitherToMaybe v

-- | Invalidate session id
clearSession :: MonadIO m => SessionSettings
  -> ByteString -- ^ Session id
  -> m ()
clearSession SessionSettings{..} sessionId = do
  connectAndRunRedis redisConnectionInfo $ do
    del [sessionId]
  return ()

-- | Create new redis backend wai session store
dbStore :: (MonadIO m1, MonadIO m2, Eq k, Serialize k, Serialize v) => SessionSettings -> m2 (SessionStore m1 k v)
dbStore s = do
  return $ dbStore' s

dbStore' :: (MonadIO m1, MonadIO m2, Eq k, Serialize k, Serialize v, Monad m2) => SessionSettings -> Maybe ByteString -> m2 (Session m1 k v, m2 ByteString)
dbStore' s (Just sesId) = do
  isValid <- isSesIdValid s sesId
  if isValid
    then return (mkSessionFromSesId s sesId, return sesId)
    else dbStore' s Nothing
dbStore' s Nothing = do
  sesId <- createSession s
  return (mkSessionFromSesId s sesId, return sesId)

mkSessionFromSesId :: (MonadIO m1, Eq k, Serialize k, Serialize v) => SessionSettings -> ByteString -> Session m1 k v
mkSessionFromSesId s sesId = (mkLookup, mkInsert)
  where
    mkLookup k = liftIO $ fmap (join . fmap (eitherToMaybe . decode)) $ lookupFromSession s sesId (encode k)
    mkInsert k v = liftIO $ insertIntoSession s sesId (encode k) (encode v)


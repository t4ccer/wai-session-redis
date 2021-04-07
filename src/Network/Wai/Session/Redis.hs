{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Wai.Session.Redis
  ( redisStore
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

data SessionSettings = SessionSettings
  { redisConnectionInfo :: ConnectInfo
  , expiratinTime       :: Integer
  -- ^ Session expiration time in seconds
  , sessionCookieName   :: ByteString
  }

instance Default SessionSettings where
  def = SessionSettings
    { redisConnectionInfo = defaultConnectInfo
    , expiratinTime       = 60*60*24*7 -- One week
    , sessionCookieName   = "SESSION_ID"
    }

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right a) = Just a

connectAndRunRedis :: ConnectInfo -> Redis b -> IO b
connectAndRunRedis ci cmd = do
  conn <- connect ci
  res  <- runRedis conn cmd
  disconnect conn
  return res

createSession :: SessionSettings -> IO ByteString
createSession SessionSettings{..} = do
  sesId <- genSessionId
  connectAndRunRedis redisConnectionInfo $ do
    hset sesId "" ""
    expire sesId expiratinTime
  return sesId

isSesIdValid :: SessionSettings -> ByteString -> IO Bool
isSesIdValid SessionSettings{..} sesId = do
  res <- connectAndRunRedis redisConnectionInfo $ do
    exists sesId
  return $ fromRight False res

insertIntoSession :: SessionSettings
  -> ByteString -- ^ Sessionn id
  -> ByteString -- ^ Key
  -> ByteString -- ^ Value
  -> IO ()
insertIntoSession SessionSettings{..} sesId key value = do
  connectAndRunRedis redisConnectionInfo $ do
    hset sesId key value
    expire sesId expiratinTime
  return ()

lookupFromSession :: SessionSettings
  -> ByteString -- ^ Session id
  -> ByteString -- ^ Key
  -> IO (Maybe ByteString)
lookupFromSession SessionSettings{..} sesId key = do
  v <- connectAndRunRedis redisConnectionInfo $ do
    v <- hget sesId key
    expire sesId expiratinTime
    return v
  return $ join $ eitherToMaybe v

-- | Invalidate session id
clearSession :: SessionSettings -- ^
  -> ByteString -- ^ Session id
  -> IO ()
clearSession SessionSettings{..} sessionId = do
  connectAndRunRedis redisConnectionInfo $ do
    del [sessionId]
  return ()

redisStore :: (MonadIO m, Eq k, Serialize k, Serialize v) => SessionSettings -> IO (SessionStore m k v)
redisStore s = do
  return $ redisStore' s

redisStore' :: (MonadIO m1, Eq k, Serialize k, Serialize v, Monad m2) => SessionSettings -> Maybe ByteString -> IO (Session m1 k v, m2 ByteString)
redisStore' s (Just sesId) = do
  isValid <- isSesIdValid s sesId
  if isValid
    then return (mkSessionFromSesId s sesId, return sesId)
    else redisStore' s Nothing
redisStore' s Nothing = do
  sesId <- createSession s
  return (mkSessionFromSesId s sesId, return sesId)

mkSessionFromSesId :: (MonadIO m, Eq k, Serialize k, Serialize v) => SessionSettings -> ByteString -> Session m k v
mkSessionFromSesId s sesId = (mkLookup, mkInsert)
  where
    mkLookup k = liftIO $ fmap (join . fmap (eitherToMaybe . decode)) $ lookupFromSession s sesId (encode k)
    mkInsert k v = liftIO $ insertIntoSession s sesId (encode k) (encode v)


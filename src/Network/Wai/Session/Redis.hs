{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Wai.Session.Redis where

import           Control.Monad
import           Data.ByteString                           (ByteString)
import           Database.Redis
import           Network.HTTP.Types
import           Network.Wai

import           Network.Wai.Session.Redis.Internal
import           Network.Wai.Session.Redis.SessionSettings

-- | Create new session and save it in Redis db. Return new session id
createSessionInRedis :: SessionSettings -- ^
  -> ByteString -- ^ Session payload
  -> IO ByteString
createSessionInRedis SessionSettings{..} payload = do
  token <- genToken
  connectAndRunRedis redisConnectionInfo $ do
    set token payload
    expire token expiratinTime
  return token

-- | Invalidate session id
clearSessionInRedis :: ConnectInfo -- ^
  -> ByteString -- ^ Session id
  -> IO ()
clearSessionInRedis ci key = do
  connectAndRunRedis ci $ do
    del [key]
  return ()

-- Get session payload from Redis db
readSession :: SessionSettings -- ^
  -> ByteString -- ^ Session id
  -> IO (Maybe ByteString)
readSession SessionSettings{..} key = do
  v <- connectAndRunRedis redisConnectionInfo $ do
    v <- get key
    expire key expiratinTime
    return v
  return $ join $ eitherToMaybe v

-- | Create new session and send it to client
createNewSession :: SessionSettings -- ^
  -> ByteString -- ^ Session payload
  -> Application
createNewSession s payload _ h = do
  t <- createSessionInRedis s payload
  h $ responseLBS status200 (createSessionHeader s t) ""

-- | Read session id from cookie and perform action with session payload. Reponds with 401 when session is invalid
withSession :: SessionSettings -- ^
  -> (ByteString -> Application) -- ^ Action with session payload
  -> Application
withSession s cmd req h = do
  let sessionId = getSessionCookie s req
  case sessionId of
    Nothing -> h $ responseLBS status401 [] "Missing session cookie"
    Just sessionId' -> do
      v <- readSession s sessionId'
      case v of
        Nothing -> h $ responseLBS status401 [] "Invalid session cookie"
        Just v' -> cmd v' req h


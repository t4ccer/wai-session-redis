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
createSession :: SessionSettings -- ^
  -> ByteString -- ^ Session payload
  -> IO ByteString
createSession SessionSettings{..} payload = do
  token <- genToken
  connectAndRunRedis redisConnectionInfo $ do
    set token payload
    expire token expiratinTime
  return token

-- | Invalidate session id
clearSession :: SessionSettings -- ^
  -> ByteString -- ^ Session id
  -> IO ()
clearSession SessionSettings{..} sessionId = do
  connectAndRunRedis redisConnectionInfo $ do
    del [sessionId]
  return ()

-- Get session payload from Redis db
readSession :: SessionSettings -- ^
  -> ByteString -- ^ Session id
  -> IO (Maybe ByteString)
readSession SessionSettings{..} sessionId = do
  v <- connectAndRunRedis redisConnectionInfo $ do
    v <- get sessionId
    expire sessionId expiratinTime
    return v
  return $ join $ eitherToMaybe v

-- | Create new session and send it to client
createSessionAndSend :: SessionSettings -- ^
  -> ByteString -- ^ Session payload
  -> Application
createSessionAndSend s payload _ h = do
  t <- createSession s payload
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


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Wai.Session.Redis where

import           Control.Monad
import           Data.ByteString                           (ByteString)
import           Data.List                                 (find)
import           Database.Redis
import           Network.HTTP.Types
import           Network.Wai
import           Web.Cookie

import           Network.Wai.Session.Redis.Internal
import           Network.Wai.Session.Redis.SessionSettings

createSessionInRedis :: SessionSettings -> ByteString -> IO ByteString
createSessionInRedis SessionSettings{..} payload = do
  token <- genToken
  connectAndRunRedis redisConnectionInfo $ do
    set token payload
    expire token expiratinTime
  return token

clearSessionInRedis :: ConnectInfo -> ByteString -> IO ()
clearSessionInRedis ci key = do
  connectAndRunRedis ci $ do
    del [key]
  return ()

readSession :: SessionSettings -> ByteString -> IO (Maybe ByteString)
readSession SessionSettings{..} key = do
  v <- connectAndRunRedis redisConnectionInfo $ do
    get key
  return $ join $ eitherToMaybe v

createNewSession :: SessionSettings -> ByteString -> Application
createNewSession s payload _ h = do
  t <- createSessionInRedis s payload
  h $ responseLBS status200 (createSessionHeader s t) ""

withSession :: SessionSettings -> (ByteString -> Application) -> Application
withSession s cmd req h = do
  let sessionId = getSessionCookie s req
  case sessionId of
    Nothing -> h $ responseLBS status401 [] "Missing session cookie"
    Just sessionId' -> do
      v <- readSession s sessionId'
      case v of
        Nothing -> h $ responseLBS status401 [] "Invalid session cookie"
        Just v' -> cmd v' req h


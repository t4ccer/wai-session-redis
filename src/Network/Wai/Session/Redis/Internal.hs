{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Wai.Session.Redis.Internal where

import           Data.ByteString                           (ByteString)
import           Data.List                                 (find)
import qualified Data.UUID                                 as UUID
import qualified Data.UUID.V4                              as UUID
import           Database.Redis
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Session.Redis.SessionSettings
import           Web.Cookie

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right a) = Just a

genToken :: IO ByteString
genToken = UUID.toASCIIBytes <$> UUID.nextRandom

connectAndRunRedis :: ConnectInfo -> Redis b -> IO b
connectAndRunRedis ci cmd = do
  conn <- connect ci
  res  <- runRedis conn cmd
  disconnect conn
  return res

createSessionHeader :: SessionSettings -> ByteString -> ResponseHeaders
createSessionHeader SessionSettings{..} t = [("Set-Cookie", sessionCookieName <> "=" <> t)]

getSessionCookie :: SessionSettings -> Request -> Maybe ByteString
getSessionCookie SessionSettings{..} req =
  let headers = requestHeaders req
      cookies = parseCookies . snd <$> find ((== hCookie) . fst) headers
      sessionCookie = cookies >>= fmap snd . find ((sessionCookieName ==) . fst)
   in sessionCookie


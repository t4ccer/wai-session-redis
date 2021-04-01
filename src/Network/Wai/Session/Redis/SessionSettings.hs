{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Wai.Session.Redis.SessionSettings where

import           Data.ByteString (ByteString)
import           Database.Redis  (ConnectInfo, defaultConnectInfo)

data SessionSettings = SessionSettings
  { redisConnectionInfo :: ConnectInfo
  , expiratinTime       :: Integer
  -- ^ Session expiration time in seconds
  , sessionCookieName   :: ByteString
  }

defaultSessionSettings :: SessionSettings
defaultSessionSettings  = SessionSettings
  { redisConnectionInfo = defaultConnectInfo
  , expiratinTime       = 60*60*24*7 -- One week
  , sessionCookieName   = "SESSION_ID"
  }


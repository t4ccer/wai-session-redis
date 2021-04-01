{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Wai.Session.Redis.SessionSettings where

import           Data.ByteString (ByteString)
import           Data.Default
import           Database.Redis  (ConnectInfo, defaultConnectInfo)

data SessionSettings = SessionSettings
  { redisConnectionInfo :: ConnectInfo
  , expiratinTime       :: Integer
  -- ^ Session expiration time in seconds
  , sessionCookieName   :: ByteString
  }

instance Default SessionSettings where
  def = defaultSessionSettings

defaultSessionSettings :: SessionSettings
defaultSessionSettings  = SessionSettings
  { redisConnectionInfo = defaultConnectInfo
  , expiratinTime       = 60*60*24*7 -- One week
  , sessionCookieName   = "SESSION_ID"
  }


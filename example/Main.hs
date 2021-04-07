{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8     as SBS
import           Data.Default
import           Data.String               (fromString)
import qualified Data.Vault.Lazy           as Vault
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Session       (Session, withSession)

import           Network.Wai.Session.Redis

app :: Vault.Key (Session IO String String) -> Application
app session env = (>>=) $ do
  u <- sessionLookup "u"
  sessionInsert "u" insertThis
  return $ responseLBS ok200 [] $ maybe (fromString "Nothing") fromString u
    where
      insertThis = show $ pathInfo env
      Just (sessionLookup, sessionInsert) = Vault.lookup session (vault env)

main :: IO ()
main = do
  let s = def
  session <- Vault.newKey
  store <- redisStore s
  run 1337 $ withSession store (fromString $ SBS.unpack $ sessionCookieName s) def session $ app session


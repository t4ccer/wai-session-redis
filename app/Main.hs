{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8                     as SBS
import qualified Data.ByteString.Lazy                      as LBS
import           Data.Default
import           Data.List
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp

import           Network.Wai.Session.Redis
import           Network.Wai.Session.Redis.SessionSettings

showPayload :: SBS.ByteString -> Application
showPayload v _ h = do
  h $ responseLBS status200 [] $ LBS.fromStrict v

login :: SessionSettings -> Application
login s req h = do
  let username = snd =<< find ((== "username") . fst) (queryString req)
  case username of
    Nothing        -> h $ responseLBS status400 [] "Username not provided"
    Just username' -> createSessionAndSend s username' req h

myApp :: SessionSettings -> Application
myApp s req h = do
  let app = case rawPathInfo req of
        "/login" -> login s
        "/show"  -> withSession s showPayload
        _        -> undefined
  app req h

main :: IO ()
main = do
  run 1337 $ myApp def


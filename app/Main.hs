{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8                     as SBS
import qualified Data.ByteString.Lazy                      as LBS
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp


import           Network.Wai.Session.Redis
import           Network.Wai.Session.Redis.SessionSettings

showCookie :: SBS.ByteString -> Application
showCookie v _ h = do
  h $ responseLBS status200 [] $ LBS.fromStrict v

myApp :: SessionSettings -> Application
myApp s req h = do
  let app = case rawPathInfo req of
        "/login" -> createNewSession s "t4ccer"
        "/show"  -> withSession s showCookie
        _        -> undefined
  app req h

main :: IO ()
main = do
  let s = defaultSessionSettings
  run 1337 $ myApp s


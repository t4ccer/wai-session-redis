-- Adapted from https://github.com/hce/postgresql-session/blob/master/test/Spec.hs
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Control.Concurrent
import qualified Data.ByteString           as B
import           Data.Default
import           Network.Wai.Session
import           Test.Hspec

import           Network.Wai.Session.Redis

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Network.Wai.Session.Redis" $ do
    it "Handles value insert" do
      store :: SessionStore IO B.ByteString B.ByteString <- dbStore testSettings
      ((lookup, insert), mkSessionId) <- store Nothing
      sessionId <- mkSessionId

      insert "foo" "foo"
      lookup "foo" `shouldReturn` Just "foo"

    it "Handles value update" do
      store :: SessionStore IO B.ByteString B.ByteString <- dbStore testSettings
      ((lookup, insert), mkSessionId) <- store Nothing
      sessionId <- mkSessionId

      insert "foo" "foo"
      lookup "foo" `shouldReturn` Just "foo"

      insert "foo" "bar"
      lookup "foo" `shouldReturn` Just "bar"

    it "Handles non-existing key" do
      store :: SessionStore IO B.ByteString B.ByteString <- dbStore testSettings
      ((lookup, insert), mkSessionId) <- store Nothing
      sessionId <- mkSessionId

      lookup "foo" `shouldReturn` Nothing

    it "Handles valid sesson id" do
      store :: SessionStore IO B.ByteString B.ByteString <- dbStore testSettings
      let invalidSessionId = "ImInvalidId"
      ((lookup, insert), mkSessionId) <- store $ Just invalidSessionId

      newSessionId <- mkSessionId
      newSessionId `shouldNotBe` invalidSessionId
      lookup "foo" `shouldReturn` Nothing
      insert "foo" "foo"

      ((lookup, insert), mkSessionId) <- store $ Just newSessionId
      newSessionId2 <- mkSessionId
      lookup "foo" `shouldReturn` Just "foo"
      newSessionId2 `shouldBe` newSessionId

    it "Handles invalid sesson id" do
      store :: SessionStore IO B.ByteString B.ByteString <- dbStore testSettings
      let invalidSessionId = "ImInvalidId"
      ((lookup, insert), mkSessionId) <- store $ Just invalidSessionId
      newSessionId <- mkSessionId
      newSessionId `shouldNotBe` invalidSessionId
      lookup "foo" `shouldReturn` Nothing

    it "Timeouts session" do
      store :: SessionStore IO B.ByteString B.ByteString <- dbStore testSettings
      ((lookup, insert), mkSessionId) <- store Nothing

      insert "foo" "foo"
      threadDelay 2000000
      lookup "foo" `shouldReturn` Nothing

    it "Clears sessions" do
      store :: SessionStore IO B.ByteString B.ByteString <- dbStore testSettings
      ((lookup, insert), mkSessionId) <- store Nothing

      insert "foo" "foo"
      lookup "foo" `shouldReturn` Just "foo"

      mkSessionId >>= clearSession testSettings
      lookup "foo" `shouldReturn` Nothing


testSettings :: SessionSettings
testSettings = def {expiratinTime = 1}


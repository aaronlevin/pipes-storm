{-# LANGUAGE OverloadedStrings #-}

module HaskellStormSpec (main, spec) where

import Data.Aeson (decode, encode, Value(Object, Number, String))
import Data.ByteString.Lazy.Char8 (pack)
import Data.HashMap.Strict (fromList)
import qualified Data.Vector as V
import HaskellStorm (BoltIn (..), Handshake (..), PidOut(..))
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "PidOut" $ do
        let pidOut = PidOut 123
        let pidString = pack "{\"pid\":123}"
        it "encodes to json string of form `{\"pid\":123}`" $ do
            (encode pidOut) `shouldBe` pidString
        it "decodes `{\"pid\":123}` json into `PidOut 123`"  $ do
            (decode pidString) `shouldBe` (Just pidOut)
        it "decodes string with extra fields appropriately" $ do
            (decode "{\"pid\":123, \"xxx\":333}") `shouldBe` (Just pidOut)

    describe "Handshake" $ do
        let stormConfig = Object $ fromList [ ("storm.config.param", Number 123)]
        let contextComponent = Object $ fromList [("1", String "spout"), ("2", String "__acker"), ("3", String "bolt")]
        let stormContext = Object $ fromList [ ("taskid", Number 3), ("task->component", contextComponent) ]
        let handshake = Handshake stormConfig stormContext "/path"
        let handshakeString = pack "{ \"conf\": { \"storm.config.param\": 123 }, \"context\": { \"task->component\": { \"1\": \"spout\", \"2\": \"__acker\", \"3\": \"bolt\" }, \"taskid\": 3 }, \"pidDir\": \"/path\" }"
        it "decodes handshakeString into the right Handshake object" $ do
            (decode handshakeString) `shouldBe` (Just handshake)

    describe "BoltIn" $ do
        let tuples = V.fromList [String "cool world", String "field1", Number 3]
        let boltIn = BoltIn "-6955786537413359385" "1" "1" 9 tuples
        let boltString = "{\"stream\":\"1\",\"comp\":\"1\",\"id\":\"-6955786537413359385\",\"task\":9,\"tuple\":[\"cool world\",\"field1\",3]}"
        it "decodes boltString appropriately" $ do
            (decode boltString) `shouldBe` (Just boltIn)
        it "encodes boltString appropriately" $ do
            (encode boltIn) `shouldBe` boltString

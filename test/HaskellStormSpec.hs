{-# LANGUAGE OverloadedStrings #-}

module HaskellStormSpec (main, spec) where

import Data.Aeson (encode, decode)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as BS
import HaskellStorm (PidOut(PidOut))
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

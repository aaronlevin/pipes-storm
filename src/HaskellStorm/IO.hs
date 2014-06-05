{-# LANGUAGE OverloadedStrings #-}

module HaskellStorm.IO
    (
        initHandshake
    ) where

import Data.Aeson (decode)
import Data.ByteString.Lazy.Char8 (pack)
import HaskellStorm.Internal (Handshake)
import HaskellStorm.System (writePid)

initHandshake :: IO ()
initHandshake = do
    initMsg  <- getLine
    case (decode (pack initMsg) :: Maybe Handshake) of
        Just handshake -> writePid
        Nothing -> putStrLn "error"


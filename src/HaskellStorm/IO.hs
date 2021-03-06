{-# LANGUAGE OverloadedStrings #-}

module HaskellStorm.IO
    (
      appendEnd
    , initHandshake
    ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson (decode, ToJSON (toJSON))
import Data.ByteString.Lazy.Char8 (pack)
import HaskellStorm.Internal (Handshake)
import HaskellStorm.System (writePid)

initHandshake :: IO ()
initHandshake = do
    initMsg  <- getLine
    case (decode (pack initMsg) :: Maybe Handshake) of
        Just handshake -> writePid
        Nothing -> putStrLn "error"

appendEnd :: MonadIO m => ToJSON a => [a] -> m ()
appendEnd = mapM_ printAndAppend

printAndAppend :: MonadIO m => ToJSON a => a -> m ()
printAndAppend a = do
    liftIO $ putStrLn (show $ toJSON a)
    liftIO $ putStrLn "end"

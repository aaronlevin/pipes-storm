{-# LANGUAGE OverloadedStrings #-}

module Pipes.Storm.IO
    (
      appendEnd
    , initHandshake
    ) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.Aeson (decode, ToJSON (toJSON))
import Data.ByteString.Lazy.Char8 (pack)
import Pipes.Storm.Internal (Handshake (Handshake), StormConfig, StormContext)
import Pipes.Storm.System (writePid)

initHandshake :: IO (StormConfig, StormContext)
initHandshake = do
    initMsg  <- getLine
    case (decode (pack initMsg)) of
        (Just (Handshake stormConfig stormContext pid)) ->
            writePid >>= (\_ -> return (stormConfig, stormContext))
        Nothing -> fail "error"

appendEnd :: (MonadIO m, ToJSON a) => [a] -> m ()
appendEnd = mapM_ printAndAppend

printAndAppend :: MonadIO m => ToJSON a => a -> m ()
printAndAppend a = do
    liftIO $ putStrLn (show $ toJSON a)
    liftIO $ putStrLn "end"

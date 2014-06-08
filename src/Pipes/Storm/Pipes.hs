{-# LANGUAGE OverloadedStrings #-}

module Pipes.Storm.Pipes ( boltHandler
    , groupUntil
    ) where

import Control.Concurrent.Async (async)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (decode)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Monoid (Monoid, mconcat)
import Pipes.Storm.IO (appendEnd)
import Pipes.Storm.Internal (BoltIn, StormOut (Log))
import Pipes.Concurrent (spawn, Buffer(Unbounded), fromInput, Input, Output, toOutput)
import Pipes ((>->), await, Consumer, Pipe, Producer, runEffect, yield)
import Pipes.Prelude (stdinLn)
import System.Mem (performGC)

groupUntil :: (Monoid a, Monad m) => (a -> Bool) -> Pipe a a m ()
groupUntil f = forever $ go f []
    where
        go g as = do
            a <- await
            if (g a)
            then yield (mconcat as)
            else go g (as ++ [a])

pipeConcat :: Monad m => (a -> Maybe b) -> Pipe a b m ()
pipeConcat f = do
    a <- await
    case (f a) of
        Just b -> yield b
        Nothing -> pipeConcat f

boltProducer :: MonadIO m => Producer BoltIn m ()
boltProducer = stdinLn >-> groupUntil (== "end") >-> pipeConcat (decode . pack)

boltHandler :: MonadIO m => (BoltIn -> m [StormOut]) -> Consumer BoltIn m ()
boltHandler f = forever $ do
    boltIn <- await
    outs <- lift $ f boltIn
    lift $ appendEnd outs

-- TODO: figure out how to make this work with `MonadIO` instead of `IO`
-- not sure how to run asyncronous computations within `MonadIO`.
pipeBolt :: Input BoltIn -> Output BoltIn -> (BoltIn -> IO [StormOut]) -> IO ()
pipeBolt input output f = do
    _ <- async $ do runEffect $ boltProducer >-> toOutput output
                    performGC
    _ <- async $ do runEffect $ fromInput input >-> boltHandler f
                    performGC
    return ()

main :: IO ()
main = do
    (boltOutput, boltInput) <- spawn Unbounded
    pipeBolt boltInput boltOutput (\_ -> return [Log "cool"])
    return ()

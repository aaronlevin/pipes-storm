{-# LANGUAGE OverloadedStrings #-}

module Pipes.Storm.Pipelines (groupUntil
    , groupUntilWithAppend
    , stormPipeProducer
    , stormPipeHandler
    , stormPipe
    ) where

import Control.Concurrent.Async (async)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (decode, FromJSON)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Monoid (Monoid, mappend, mconcat)
import Pipes.Storm.IO (appendEnd)
import Pipes.Storm.Internal (StormOut)
import Pipes.Concurrent (fromInput, Input, Output, toOutput)
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

groupUntilWithAppend :: (Monoid a, Monad m) => (a -> Bool) -> a -> Pipe a a m ()
groupUntilWithAppend f a = forever $ go f a []
    where
        go g end as = do
            nextA <- await
            if (g nextA)
            then yield (mconcat as)
            else go f end (as ++ [mappend nextA end])

pipeConcat :: Monad m => (a -> Maybe b) -> Pipe a b m ()
pipeConcat f = do
    a <- await
    case (f a) of
        Just b -> yield b
        Nothing -> pipeConcat f

-- producer of bolts or spouts (a ~ BoltIn or SpoutIn)
stormPipeProducer :: (MonadIO m, FromJSON a) => Producer a m ()
stormPipeProducer = stdinLn >-> groupUntilWithAppend (== "end") "\n" >-> pipeConcat (decode . pack)

-- producer a consumer of bolts or spouts (a ~ BoltIn or SpoutIn)
stormPipeHandler :: (MonadIO m) => (a -> m [StormOut]) -> Consumer a m ()
stormPipeHandler f = forever $ do
    stormIn <- await
    outs <- lift $ f stormIn
    lift $ appendEnd outs

-- TODO: how to abstract IO to MonadIO
stormPipe :: (FromJSON a) => Input a -> Output a -> (a -> IO [StormOut]) -> IO ()
stormPipe input output f = do
    _ <- async $ do runEffect $ stormPipeProducer >-> toOutput output
                    performGC
    _ <- async $ do runEffect $ fromInput input >-> stormPipeHandler f
                    performGC
    return ()

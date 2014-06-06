{-# LANGUAGE OverloadedStrings #-}

module HaskellStorm.Pipes (
    boltHandler
    ) where

import Control.Concurrent.Async (async)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (fromJSON)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Monoid (Monoid, mconcat)
import HaskellStorm.IO (appendEnd)
import HaskellStorm.Internal (BoltIn, StormOut (Log))
import Pipes.Concurrent (spawn, Buffer(Unbounded), fromInput, toOutput)
import Pipes ((>->), await, Consumer, Pipe, Producer, runEffect, yield)
import Pipes.Prelude (map, stdinLn, takeWhile)
import Prelude hiding (map, takeWhile)
import System.Mem (performGC)

-- chunker :: Monad m => (a -> Bool) -> Pipe a [a] m ()
-- chunker f = go f 

grouper :: (Monoid a, Monad m) => (a -> Bool) -> Pipe a a m ()
grouper f = go f ([] :: [a])
    where
        --go :: Monoid a => (a -> Bool) -> [a] -> Pipe a a m ()
        go f as = do
            elem <- await
            if (f elem)
            then yield (mconcat as)
            else go f (as ++ [elem])



-- boltProducer :: Producer BoltIn IO r
-- boltProducer = stdinLn >-> takeWhile (/= "end") >-> map (\s -> fromJSON (pack s))

boltHandler :: MonadIO m => (BoltIn -> m [StormOut]) -> Consumer BoltIn m ()
boltHandler f = forever $ do
    boltIn <- await
    outs <- lift $ f boltIn
    lift $ appendEnd outs

main = do
    (boltOutput, boltInput) <- spawn Unbounded
    async $ do runEffect $ fromInput boltInput >-> boltHandler (\_ -> return [Log "cool"])
               performGC
    -- async $ do runEffect $ boltProducer >-> toOutput boltOutput
      --         performGC
    return ()

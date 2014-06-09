{-# LANGUAGE OverloadedStrings #-}

module Pipes.Storm (
      BoltIn (BoltIn)
    , Handshake (Handshake)
    , PidOut (PidOut)
    , SpoutIn (SpoutAck, SpoutNext, SpoutFail)
    , StormConfig
    , StormContext
    , StormOut (Ack, Emit, Fail, Log)
    , initHandshake
    , stormBolt
    , stormSpout
    , writePid
  ) where

import Pipes.Concurrent (Buffer (Unbounded), Input, Output, spawn)
import Pipes.Storm.Internal (
        BoltIn (..)
      , Handshake (..)
      , PidOut (..)
      , SpoutIn (SpoutAck, SpoutNext, SpoutFail)
      , StormConfig
      , StormContext
      , StormOut (Ack, Emit, Fail, Log)
    )
import Pipes.Storm.System (writePid)
import Pipes.Storm.IO (initHandshake)
import Pipes.Storm.Pipelines (stormPipe)

stormBolt :: Input BoltIn -> Output BoltIn -> (BoltIn -> IO [StormOut]) -> IO ()
stormBolt i o f = stormPipe i o f

mainBolt :: Input BoltIn -> Output BoltIn -> (StormContext -> StormConfig -> BoltIn -> IO [StormOut]) -> IO ()
mainBolt i o f = do
    (stormConfig, stormContext) <- initHandshake
    stormBolt i o (f stormConfig stormContext)

mainUnboundedBolt :: (StormContext -> StormConfig -> BoltIn -> IO [StormOut]) -> IO ()
mainUnboundedBolt f = do
    (stormConfig, stormContext) <- initHandshake
    (output, input) <- spawn Unbounded
    stormBolt input output (f stormConfig stormContext)

stormSpout :: Input SpoutIn -> Output SpoutIn -> (SpoutIn -> IO [StormOut]) -> IO ()
stormSpout i o f = stormPipe i o f

mainSpout :: Input SpoutIn -> Output SpoutIn -> (StormContext -> StormConfig -> SpoutIn -> IO [StormOut]) -> IO ()
mainSpout i o f = do
    (stormConfig, stormContext) <- initHandshake
    stormSpout i o (f stormContext stormConfig)

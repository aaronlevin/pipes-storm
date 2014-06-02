{-# LANGUAGE OverloadedStrings #-}

module HaskellStorm.Internal
    (
        PidOut(..)
      , StormConfig(..)
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson ((.:), (.:?), (.=), decode, FromJSON (..), object, ToJSON(..), Value(..))
import qualified Data.ByteString.Lazy.Char8 as BS

data StormConfig = StormConfig { getConfigName :: String } deriving (Show, Eq)

instance FromJSON StormConfig where
    parseJSON (Object v) = 
        StormConfig <$> v .: "name"
    parseJSON _ = mzero

instance ToJSON StormConfig where
    toJSON (StormConfig name) = object [ "name" .= name ]

------

data StormContext = StormContext { getContextName :: String } deriving (Show, Eq)

instance FromJSON StormContext where
    parseJSON (Object v) = 
        StormContext <$> v .: "name"
    parseJSON _ = mzero

instance ToJSON StormContext where
    toJSON (StormContext name) = object [ "name" .= name ]

-----

data PidOut = PidOut { getPid :: Integer } deriving (Show, Eq)

instance FromJSON PidOut where
    parseJSON (Object v) = 
        PidOut <$> v .: "pid"
    parseJSON _ = mzero

instance ToJSON PidOut where
    toJSON (PidOut pid) = object [ "pid" .= pid ]

-----

data Handshake = Handshake {
                 getConf :: StormConfig
               , getContext :: StormContext
               , getPidDir :: String
               } deriving (Show, Eq)


instance FromJSON Handshake where
    parseJSON (Object v) = 
        Handshake <$> v .: "conf"
                  <*> v .: "context"
                  <*> v .: "pidDir"
    parseJSON _ = mzero

instance ToJSON Handshake where
    toJSON (Handshake config context pidDir) = 
        object [ "conf" .= toJSON config
               , "context" .= toJSON context
               , "pidDir" .= pidDir
               ]

-----

data StormCommand = Next | Ack | Fail deriving (Show, Eq)
data SpoutIn = SpoutIn {
               getCommand :: StormCommand
             , getId :: Maybe String
             } 

-----

data SpoutEmit = SpoutEmit {
                 emitCommand :: StormCommand
               , emitTupleId :: Maybe String
               , emitStreamId :: Maybe String
               , emitTaskId :: Maybe Integer
               , emitTuple :: [String]
               } deriving (Show, Eq)

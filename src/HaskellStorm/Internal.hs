{-# LANGUAGE OverloadedStrings #-}

module HaskellStorm.Internal
    ( BoltIn (..)
    , Handshake (..)
    , PidOut (..)
    , StormConfig (..)
    , StormCommand (..)
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson ((.:), (.:?), (.=), Array (..), decode, FromJSON (..), object, ToJSON(..), Value(..))
import qualified Data.Aeson as A
import Data.Text (Text)
import qualified Data.ByteString.Lazy.Char8 as BS

type StormConfig = Value
type StormContext = Value
type StormTuples = Array

-----

data Handshake = Handshake { getConf :: StormConfig
                           , getContext :: StormContext
                           , getPidDir :: Text
                           } deriving (Eq, Show)

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

---

data PidOut = PidOut { getPid :: Integer } deriving (Show, Eq)

instance FromJSON PidOut where
    parseJSON (Object v) = 
        PidOut <$> v .: "pid"
    parseJSON _ = mzero

instance ToJSON PidOut where
    toJSON (PidOut pid) = object [ "pid" .= pid ]

---

data StormCommand = Next | Ack | Fail deriving (Show, Eq)

instance FromJSON StormCommand where
    parseJSON (A.String "next") = return Next
    parseJSON (A.String "ack")  = return Ack
    parseJSON (A.String "fail") = return Fail
    parseJSON _                 = mzero

instance ToJSON StormCommand where
    toJSON Next = A.String "next"
    toJSON Ack  = A.String "ack"
    toJSON Fail = A.String "fail"

---

data BoltIn = BoltIn { tupleId :: Text
                     , boltComponent :: Text
                     , boltStream :: Text
                     , boltTask :: Integer
                     , inputTuples :: StormTuples
                     } deriving (Eq, Show)

instance FromJSON BoltIn where
    parseJSON (Object v) =
        BoltIn <$> v .: "id"
               <*> v .: "comp"
               <*> v .: "stream"
               <*> v .: "task"
               <*> v .: "tuple"
    parseJSON _ = mzero

instance ToJSON BoltIn where
    toJSON (BoltIn tupleId boltComponent boltStream boltTask inputTuples) = 
        object [ "id" .= toJSON tupleId
               , "comp" .= toJSON boltComponent
               , "stream" .= toJSON boltStream
               , "task" .= toJSON boltTask
               , "tuple" .= toJSON inputTuples
               ]

--- 

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

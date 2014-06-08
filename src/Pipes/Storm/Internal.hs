{-# LANGUAGE OverloadedStrings #-}

module Pipes.Storm.Internal
    ( BoltIn (..)
    , EmitCommand (..)
    , Handshake (..)
    , PidOut (..)
    , SpoutIn (..)
    , StormConfig
    , StormContext
    , StormOut (..)
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson ((.:), (.=), Array, FromJSON (..), Object, object, ToJSON(..), Value(..))
import qualified Data.Aeson as A
import Data.Aeson.Types (Parser)
import Data.Maybe (catMaybes)
import Data.Scientific (scientific)
import Data.Text (Text)
import Data.Vector (fromList)

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

data EmitCommand = EmitNext | EmitAck | EmitFail deriving (Show, Eq)

instance FromJSON EmitCommand where
    parseJSON (A.String "next") = return EmitNext
    parseJSON (A.String "ack")  = return EmitAck
    parseJSON (A.String "fail") = return EmitFail
    parseJSON _                 = mzero

instance ToJSON EmitCommand where
    toJSON EmitNext = A.String "next"
    toJSON EmitAck  = A.String "ack"
    toJSON EmitFail = A.String "fail"

---

data BoltIn = BoltIn Text -- tuple Id
                     Text -- bolt Component
                     Text -- bolt stream
                     Integer -- bolt task
                     StormTuples -- input tuples
                     deriving (Eq, Show)

instance FromJSON BoltIn where
    parseJSON (Object v) =
        BoltIn <$> v .: "id"
               <*> v .: "comp"
               <*> v .: "stream"
               <*> v .: "task"
               <*> v .: "tuple"
    parseJSON _ = mzero

instance ToJSON BoltIn where
    toJSON (BoltIn tupleId boltComponent boltStream boltInTask inputTuples) =
        object [ "id" .= toJSON tupleId
               , "comp" .= toJSON boltComponent
               , "stream" .= toJSON boltStream
               , "task" .= toJSON boltInTask
               , "tuple" .= toJSON inputTuples
               ]

---

data StormOut = Emit [Text] (Maybe Text) (Maybe Integer) StormTuples
              | Ack  Text
              | Fail Text
              | Log  Text
              deriving (Eq, Show)

instance ToJSON StormOut where
    toJSON (Emit anchors stream task tuples) =
        object $ [ "anchors" .= fromList anchors
                 , "command" .= A.String "emit"
                 , "tuple" .= tuples
                 ] ++ catMaybes [ ("stream" .=) . A.String <$> stream
                                , ("task" .=) <$> (flip scientific 0) <$> task ]
    toJSON (Ack ackTupleId) =
        object $ [ "command" .= A.String "ack"
                 , "id" .= A.String ackTupleId ]
    toJSON (Fail failTupleId) =
        object $ [ "command" .= A.String "fail"
                 , "id" .= A.String failTupleId ]
    toJSON (Log msg) =
        object $ [ "command" .= A.String "log"
                 , "msg" .= A.String msg ]

--- 

data SpoutIn = SpoutNext
             | SpoutAck { spoutAckId :: Text }
             | SpoutFail { spoutFailId :: Text }
             deriving (Eq, Show)

instance FromJSON SpoutIn where
    parseJSON (Object v) =
        (v .: "command") >>= (go v)
            where
                go :: Object -> Text -> Parser SpoutIn
                go o t = case t of
                    "next" -> return SpoutNext
                    "ack"  -> SpoutAck <$> o .: "id"
                    "fail" -> SpoutFail <$> o .: "id"
                    _      -> mzero
    parseJSON _          = mzero

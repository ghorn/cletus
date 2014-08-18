{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# Language StandaloneDeriving #-}

module Main ( main ) where

import Control.Monad ( forever )
import GHC.Generics ( Generic )
import qualified Text.ProtocolBuffers as PB
--import qualified System.Remote.Monitoring as EKG

import PlotHo -- ( Lookup(..), SignalTree(..), runPlotter, addChannel, makeSignalTree )
import Channels
import qualified ZmqHelpers as ZMQ

import qualified Messages.Xyz as Msg
import qualified Messages.SimTelem as Msg
import qualified Messages.AcState as Msg
import qualified Messages.Dcm as Msg
import qualified Messages.Actuators as Msg
import qualified Messages.Timestamp as Msg

deriving instance Generic Msg.SimTelem
deriving instance Generic Msg.AcState
deriving instance Generic Msg.Dcm
deriving instance Generic Msg.Xyz
deriving instance Generic Msg.Actuators
deriving instance Generic Msg.Timestamp
instance Lookup Msg.SimTelem
instance Lookup Msg.AcState
instance Lookup Msg.Dcm
instance Lookup Msg.Xyz
instance Lookup Msg.Actuators
instance Lookup Msg.Timestamp

instance Lookup (PB.Seq PB.Utf8) where
  toAccessorTree _ _ = Data ("Utf8","Utf8") []

st :: SignalTree Msg.SimTelem
st = makeSignalTree PB.defaultValue

lol :: (PB.ReflectDescriptor a, PB.Wire a)
       => ZMQ.Context -> String -> String
       -> (a -> IO ())
       -> (SignalTree a -> IO ())
       -> IO ()
lol context channel messageName newMessage _ =
  ZMQ.withSubscriber context channel messageName $ \receive ->
    forever $ do
      msg <- receive
      newMessage $ case ZMQ.decodeProto msg of
        Left err -> error err
        Right woo -> woo

main :: IO ()
main = ZMQ.withContext $ \ctx -> do
--  ekgTid <- fmap EKG.serverThreadId $ EKG.forkServer "localhost" 8000
  runPlotter $ do
    addChannel "sim telemetry" st (lol ctx chanSimTelem "sim_telemetry")
--    addChannel "pos" st (\w _ -> channelWriter 60000 incrementXyz xyz0 w)

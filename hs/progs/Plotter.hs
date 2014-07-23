{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleInstances #-}
{-# Language StandaloneDeriving #-}

module Main ( main ) where

import Control.Monad ( forever, unless )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS8
import GHC.Generics ( Generic )
import qualified Text.ProtocolBuffers as PB
import qualified System.ZMQ4 as ZMQ
--import qualified System.Remote.Monitoring as EKG

import PlotHo -- ( Lookup(..), SignalTree(..), runPlotter, addChannel, makeSignalTree )
import Channels

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
  ZMQ.withSocket context ZMQ.Sub $ \subscriber -> do
    ZMQ.connect subscriber channel
    ZMQ.subscribe subscriber (BS8.pack messageName)
    forever $ do
      messageName'':msg <- ZMQ.receiveMulti subscriber :: IO [BS.ByteString]
      let messageName' = BS8.unpack messageName''
      unless (messageName' == messageName) $ error $ "bad messageName: " ++ messageName'
      let cs = case PB.messageGet (BSL.concat (map BSL.fromStrict msg)) of
            Left err -> error err
            Right (cs',_) -> cs'
      newMessage cs

main :: IO ()
main = ZMQ.withContext $ \ctx -> do
--  ekgTid <- fmap EKG.serverThreadId $ EKG.forkServer "localhost" 8000
  runPlotter $ do
    addChannel "sim telemetry" st (lol ctx chanSimTelem "sim_telemetry")
--    addChannel "pos" st (\w _ -> channelWriter 60000 incrementXyz xyz0 w)

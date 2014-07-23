{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveDataTypeable #-}

module Main ( main ) where

import qualified Control.Concurrent as CC
import Control.Monad ( forever )
import Data.Vector ( Vector )

import Dyno.Server.Server ( runPlotter, newChannel )
import Dyno.DirectCollocation.Dynamic

import Channels
import qualified ZmqHelpers as Zmq

sub :: ((DynCollTraj (Vector Double), CollTrajMeta) -> IO ()) -> IO ()
sub writeChan = Zmq.withContext $ \context ->
  Zmq.withSubscriber context chanDynoPlot "dynoplot" $ \receive ->
    forever $ do
      msg <- receive
      let decoded :: (DynCollTraj (Vector Double), CollTrajMeta)
          decoded = case Zmq.decodeSerial msg of
            Left err -> error err
            Right t -> t
      writeChan decoded

main :: IO ()
main = do
  (c0, writeMe) <- newChannel "glider optimizer"

  listenerTid0 <- CC.forkIO (sub writeMe)
  runPlotter c0 [listenerTid0]

{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}

module Main ( main ) where

import Text.Printf
import qualified Data.Sequence as DS
import qualified System.ZMQ4 as ZMQ
import qualified Control.Concurrent as CC
import Control.Monad ( forever )
import Linear hiding ( cross )
import System.Clock
import qualified Text.ProtocolBuffers as PB

import qualified Messages.Rc as Msg
import qualified Messages.SimTelem as Msg

import Model.Aircraft
import Model.AeroCoeffs
import Model.Betty
import qualified ZmqHelpers as Zmq

import Channels ( chanSensors, chanSimTelem, chanRc )
import MsgHelpers

import SpatialMathT

rk4 :: (Fractional a, Additive x) => (x a -> x a) -> a -> x a -> x a
rk4 f h x0 = x0 ^+^ (k1 ^+^ 2 *^ k2 ^+^ 2 *^ k3 ^+^ k4) ^/ 6
  where
    k1 = (f  x0)         ^* h
    k2 = (f (x0 ^+^ k1^/2)) ^* h
    k3 = (f (x0 ^+^ k2^/2)) ^* h
    k4 = (f (x0 ^+^ k3))   ^* h

r_b2m_b :: Fractional a => V3T B a
r_b2m_b = V3T $ V3 0.0 0.0 0.0

dcm_b2m :: Fractional a => Rot B M (M33 a)
dcm_b2m = Rot eye3

getSensors :: forall a . (Floating a, Conjugate a) => AcX a -> AcU a -> Sensors a
getSensors x u = Sensors { y_gyro = rot dcm_b2m (ac_w_bn_b x)
                         , y_accel = rot dcm_b2m accel_b
                         , y_gps_pos = ac_r_n2b_n x
                         , y_gps_vel = rot' (ac_R_n2b x) (ac_v_bn_b x)
                         }
  where
    (x', v_bn_b') = bettyOde x u
    w_bn_b' = ac_w_bn_b x'
    w_bn_b = ac_w_bn_b x

    dcm_n2b = ac_R_n2b x

    g :: V3T N a
    g = V3T (V3 0 0 9.81)

    accel_b = (v_bn_b' - rot dcm_n2b g) + w_bn_b' `cross` r_b2m_b + w_bn_b `cross` (w_bn_b `cross` r_b2m_b)


bettyOde :: Floating a => AcX a -> AcU a -> (AcX a, V3T B a)
bettyOde = aircraftOde (bettyMass, V3T (fmap V3T bettyInertia)) bettyFc bettyMc bettyRefs

integrate :: (Floating a, Additive AcX) => a -> AcX a -> AcU a -> AcX a
integrate h x0 u = AcX z0 z1 (orthonormalize z2) z3
  where
    AcX z0 z1 z2 z3 = rk4 (fst . flip bettyOde u) h x0

simX0 :: Num a => AcX a
simX0 =
  AcX { ac_r_n2b_n = V3T $ V3 0 0 0
      , ac_v_bn_b = V3T $ V3 20 0 0
      , ac_R_n2b = Rot $ eye3
      , ac_w_bn_b = V3T $ V3 0 0 0
      }

rcThread :: IO (Either String Msg.Rc) -> CC.MVar Msg.Rc -> IO ()
rcThread receiveRc latestRc = do
  let receive = do
        msg <- receiveRc
        let rc :: Msg.Rc
            rc = case msg of
              Left err -> error err
              Right rc' -> rc'
        return rc

  receive >>= CC.putMVar latestRc
  forever (receive >>= CC.swapMVar latestRc)


main :: IO ()
main =
  Zmq.withContext $ \context ->
  ZMQ.withSocket context ZMQ.Push $ \sensorPublisher ->
  Zmq.withPublisher context chanSimTelem $ \sendSimTelem ->
  Zmq.withSubscriber context chanRc "rc" $ \receiveRc -> do
    ZMQ.connect sensorPublisher chanSensors

    -- spawn a thread updating the latest RC message
    latestRc <- CC.newEmptyMVar
    _ <- CC.forkIO (rcThread (fmap Zmq.decodeProto receiveRc) latestRc)
    let getLatestRc = CC.readMVar latestRc

    let ts = 0.002
        go :: Double -> AcX Double -> IO ()
        go t0 x0 = do
          rc <- getLatestRc
          let u = AcU (ControlSurfaces
                       { csElev = (10*pi/180) * (Msg.rcPitch rc)
                       , csRudder = (10*pi/180) * (Msg.rcYaw rc)
                       , csAil = (10*pi/180) * (Msg.rcRoll rc)
                       , csFlaps = 0
                       })
          clock <- getTime Monotonic
          let y = getSensors x0 u
              yc = toCSensors y clock
              simTelem = Msg.SimTelem
                         { Msg.state = toState x0
                         , Msg.actuators = toActuators u
                         , Msg.messages = DS.fromList
                                          [ PB.fromString (printf "sim time: %.3f" t0)
                                          ]
                         , Msg.w0 = 0
                         }
          ZMQ.sendMulti sensorPublisher (NE.fromList [BSL.toStrict (PB.messagePut yc)])
          sendSimTelem "sim_telemetry" (Zmq.encodeProto simTelem)

          let x1 = integrate ts x0 u
          print clock
          CC.threadDelay (round (ts*1e6))
          go (t0 + ts) x1

    go 0 simX0

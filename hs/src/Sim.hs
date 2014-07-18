{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}

module Main ( main ) where

import Text.Printf
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.ByteString.Char8 ( pack )
import qualified Data.List.NonEmpty as NE
import qualified Data.Sequence as DS
import qualified System.ZMQ4 as ZMQ
import qualified Control.Concurrent as CC
import Control.Monad ( unless )
import Linear hiding ( cross )
import System.Clock
import qualified Text.ProtocolBuffers as PB

import qualified Messages.Rc as Msg
import qualified Messages.Sensors as Msg
import qualified Messages.Timestamp as Msg
import qualified Messages.Xyz as Msg
import qualified Messages.SimTelem as Msg
import qualified Messages.AcState as Msg
import qualified Messages.Actuators as Msg
import qualified Messages.Dcm as Msg

import Aircraft
import AeroCoeffs
import Betty

import Channels ( chanSensors, chanSimTelem, chanRc )

import SpatialMathT

data M

toTimestamp :: TimeSpec -> Msg.Timestamp
toTimestamp t =
  Msg.Timestamp
  { Msg.tsec = fromIntegral $ sec t
  , Msg.tnsec = fromIntegral $ nsec t
  }

toState :: AcX Double -> Msg.AcState
toState x =
  Msg.AcState
  { Msg.r_n2b_n = toXyz (ac_r_n2b_n x)
  , Msg.v_bn_b = toXyz (ac_v_bn_b x)
  , Msg.dcm_n2b = toDcmMsg (ac_R_n2b x)
  , Msg.w_bn_b = toXyz (ac_w_bn_b x)
  }

toActuators :: AcU Double -> Msg.Actuators
toActuators u =
  Msg.Actuators
  { Msg.flaps = csFlaps cs
  , Msg.ail = csAil cs
  , Msg.rudd = csRudder cs
  , Msg.elev = csElev cs
  , Msg.start = Msg.Timestamp 0 0
  , Msg.stop = Msg.Timestamp 0 0
  }
  where
    cs = acSurfaces u

toXyz :: Real a => V3T f a -> Msg.Xyz
toXyz xyz = Msg.Xyz x y z
  where
    V3T (V3 x y z) = fmap realToFrac xyz

toDcmMsg :: Real a => Rot N B (M33 a) -> Msg.Dcm
toDcmMsg (Rot xyz) = Msg.Dcm (toXyz x) (toXyz y) (toXyz z)
  where
    V3 x y z = fmap V3T xyz

toCSensors :: forall a . Real a => Sensors a -> TimeSpec -> Msg.Sensors
toCSensors y ts =
  Msg.Sensors
  { Msg.timestamp = toTimestamp ts
  , Msg.gyro = toXyz $ y_gyro y
  , Msg.accel = toXyz $ y_accel y
  , Msg.gps_pos = toXyz $ y_gps_pos y
  , Msg.gps_vel = toXyz $ y_gps_vel y
  }


data Sensors a =
  Sensors
  { y_gyro :: V3T M a
  , y_accel :: V3T M a
  , y_gps_pos :: V3T N a
  , y_gps_vel :: V3T N a
  }

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

simX0 :: AcX Double
simX0 =
  AcX { ac_r_n2b_n = V3T $ V3 0 0 0
      , ac_v_bn_b = V3T $ V3 20 0 0
      , ac_R_n2b = Rot $ eye3
      , ac_w_bn_b = V3T $ V3 0 0 0
      }

orthonormalize :: Floating a => Rot f1 f2 (M33 a) -> Rot f1 f2 (M33 a)
orthonormalize (Rot (V3
                     (V3 m00 m01 m02)
                     (V3 m10 m11 m12)
                     (V3 m20 m21 m22))) = Rot ret
  where
    -- compute q0
    fInvLength0 = 1.0/sqrt(m00*m00 + m10*m10 + m20*m20)

    m00' = m00*fInvLength0
    m10' = m10*fInvLength0
    m20' = m20*fInvLength0

    -- compute q1
    fDot0' = m00'*m01 + m10'*m11 + m20'*m21

    m01' = m01 - fDot0'*m00'
    m11' = m11 - fDot0'*m10'
    m21' = m21 - fDot0'*m20'

    fInvLength1 = 1.0/sqrt(m01'*m01' + m11'*m11' + m21'*m21')

    m01'' = m01' * fInvLength1
    m11'' = m11' * fInvLength1
    m21'' = m21' * fInvLength1

    -- compute q2
    fDot1 = m01''*m02 + m11''*m12 + m21''*m22
    fDot0 = m00'*m02 + m10'*m12 + m20'*m22

    m02' = m02 - (fDot0*m00' + fDot1*m01'')
    m12' = m12 - (fDot0*m10' + fDot1*m11'')
    m22' = m22 - (fDot0*m20' + fDot1*m21'')

    fInvLength2 = 1.0/sqrt(m02'*m02' + m12'*m12' + m22'*m22')

    m02'' = m02' * fInvLength2
    m12'' = m12' * fInvLength2
    m22'' = m22' * fInvLength2

    ret = (V3
           (V3 m00' m01'' m02'')
           (V3 m10' m11'' m12'')
           (V3 m20' m21'' m22''))

main :: IO ()
main =
  ZMQ.withContext $ \context ->
  ZMQ.withSocket context ZMQ.Push $ \sensorPublisher ->
  ZMQ.withSocket context ZMQ.Pub $ \simTelemPublisher -> do
  ZMQ.withSocket context ZMQ.Sub $ \rcSub -> do
    ZMQ.connect sensorPublisher chanSensors
    ZMQ.bind simTelemPublisher chanSimTelem
    ZMQ.connect rcSub chanRc
    ZMQ.subscribe rcSub (pack "rc")

    let --u = AcU (ControlSurfaces 0 0 0 0)
        ts = 0.002
        go :: Double -> AcX Double -> IO ()
        go t0 x0 = do
          putStrLn "listening for msg"
          channel':msg <- ZMQ.receiveMulti rcSub :: IO [BS.ByteString]
          unless (channel' == pack "rc") $ error $ "bad channel"
          let rc :: Msg.Rc
              rc = case PB.messageGet (BSL.concat (map BSL.fromStrict msg)) of
                Left err -> error err
                Right (rc',_) -> rc'
              u = AcU (ControlSurfaces
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
          ZMQ.sendMulti simTelemPublisher (NE.fromList [pack "sim_telemetry",
                                                        BSL.toStrict (PB.messagePut simTelem)])

          let x1 = integrate ts x0 u
          print clock
          CC.threadDelay (round (ts*1e6))
          go (t0 + ts) x1

    go 0 simX0

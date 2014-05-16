{-# OPTIONS_GHC -Wall #-}
{-# Language FlexibleContexts #-}
{-# Language ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString as BS
import Data.ByteString.Char8 ( pack )
import Data.ByteString.Unsafe
import Data.Serialize
import qualified Data.List.NonEmpty as NE
import qualified System.ZMQ4 as ZMQ
import qualified Control.Concurrent as CC
import Linear
import System.Clock
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.Ptr

import Dyno.Vectorize

import Aircraft
import AeroCoeffs
import Betty
import Structs.Structures

toTimestamp :: TimeSpec -> C'timestamp_t
toTimestamp t = C'timestamp_t { c'timestamp_t'tsec = fromIntegral $ sec t
                              , c'timestamp_t'tnsec = fromIntegral $ nsec t
                              }
toXyz :: Real a => V3 a -> C'xyz_t
toXyz xyz = C'xyz_t x y z
  where
    V3 x y z = fmap realToFrac xyz

toCSensors :: forall a . Real a => Sensors a -> TimeSpec -> C'sensors_t
toCSensors y ts =
  C'sensors_t { c'sensors_t'timestamp = toTimestamp ts
              , c'sensors_t'gyro = toXyz $ y_gyro y
              , c'sensors_t'accel = toXyz (V3 0 0 (0::a))
              , c'sensors_t'gps_pos = toXyz $ y_gps_pos y
              , c'sensors_t'gps_vel = toXyz $ y_gps_vel y
              }

data Sensors a =
  Sensors
  { y_gyro :: V3 a
--  , y_accel :: V3 a
  , y_gps_pos :: V3 a
  , y_gps_vel :: V3 a
  }

rk4 :: (Fractional a, Additive x) => (x a -> x a) -> a -> x a -> x a
rk4 f h x0 = x0 ^+^ (k1 ^+^ 2 *^ k2 ^+^ 2 *^ k3 ^+^ k4) ^/ 6
  where
    k1 = (f  x0)         ^* h
    k2 = (f (x0 ^+^ k1^/2)) ^* h
    k3 = (f (x0 ^+^ k2^/2)) ^* h
    k4 = (f (x0 ^+^ k3))   ^* h


getSensors :: (Num a, Conjugate a) => AcX a -> AcU a -> Sensors a
getSensors x _ = Sensors { y_gyro = ac_w_bn_b x
--                         , y_accel = ac_v_bn_bac_w_bn_b x
                         , y_gps_pos = ac_r_n2b_n x
                         , y_gps_vel = (adjoint (ac_R_n2b x)) !* (ac_v_bn_b x)
                         }
  where
    --x' = bettyOde x u

bettyOde :: Floating a => AcX a -> AcU a -> AcX a
bettyOde = aircraftOde (bettyMass, bettyInertia) bettyFc bettyMc bettyRefs

integrate :: (Floating a, Additive AcX) => a -> AcX a -> AcU a -> AcX a
integrate h x0 u = AcX z0 z1 (orthonormalize z2) z3
  where
    AcX z0 z1 z2 z3 = rk4 (flip bettyOde u) h x0

simX0 :: AcX Double
simX0 =
  AcX { ac_r_n2b_n = V3 0 0 0
      , ac_v_bn_b = V3 20 0 0
      , ac_R_n2b = eye3
      , ac_w_bn_b = V3 0 0 0
      }

orthonormalize :: Floating a => M33 a -> M33 a
orthonormalize (V3
                (V3 m00 m01 m02)
                (V3 m10 m11 m12)
                (V3 m20 m21 m22)) = ret
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
main = withCallback "ipc:///tmp/sensors" $ \send -> do
  let u = AcU (ControlSurfaces 0 0 0 0)
      ts = 0.002
      go :: AcX Double -> IO ()
      go x0 = do
        clock <- getTime Monotonic
        let y = getSensors x0 u
            yc = toCSensors y clock
        ycb <- unsafeToByteString yc
        send ycb
        let x1 = integrate ts x0 u
        print clock
        CC.threadDelay (round (ts*1e6))
        go x1
  go simX0
  return ()


unsafeToByteString :: Storable a => a -> IO BS.ByteString
unsafeToByteString x = do
  px <- new x
  unsafePackMallocCStringLen (castPtr px, sizeOf x)

callback :: ZMQ.Socket ZMQ.Push -> BS.ByteString -> IO ()
callback publisher stuff = ZMQ.sendMulti publisher (NE.fromList [stuff])

withCallback :: String -> ((BS.ByteString -> IO ()) -> IO ()) -> IO ()
withCallback url userFun =
  ZMQ.withContext $ \context ->
    ZMQ.withSocket context ZMQ.Push $ \publisher ->
      ZMQ.connect publisher url >> userFun (callback publisher)

{-# OPTIONS_GHC -Wall #-}

module MsgHelpers
       ( Sensors(..)
       , M
       , toTimestamp
       , toState
       , toActuators
       , toXyz
       , toDcmMsg
       , toCSensors
       ) where

import Linear hiding ( cross )
import System.Clock

--import qualified Messages.Rc as Msg
import qualified Protobetty.Gps as MsgGps
import qualified Protobetty.Sensors.Type as MsgType
import qualified Protobetty.GpsData as MsgGpsData
import qualified Protobetty.IMU as MsgIMU
import qualified Protobetty.Sensors as Msg
import qualified Protobetty.Timestamp as Msg
import qualified Protobetty.Xyz as Msg
import qualified Protobetty.AcState as Msg
import qualified Protobetty.Actuators as Msg
import qualified Protobetty.Dcm as Msg

import Model.Aircraft
import Model.AeroCoeffs

import SpatialMathT

-- | imu frame
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
  , Msg.timestamp = Msg.Timestamp 0 0
  , Msg.timestamp_sensors = Just $ Msg.Timestamp 0 0
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

toCSensors :: Real a => Sensors a -> TimeSpec -> Msg.Sensors
toCSensors y ts =
  Msg.Sensors
  { Msg.type' = MsgType.IMU_GPS
  , Msg.imu = MsgIMU.IMU { MsgIMU.sequenceNumber = 0
                         , MsgIMU.timestamp = toTimestamp ts
                         , MsgIMU.gyro = toXyz $ y_gyro y
                         , MsgIMU.accel = toXyz $ y_accel y
                         , MsgIMU.mag = Msg.Xyz 0 0 0
                            }
  , Msg.gps = Just $
              MsgGps.Gps { MsgGps.timestamp = toTimestamp ts
                         , MsgGps.position = MsgGpsData.GpsData
                                             { MsgGpsData.time = 0
                                             , MsgGpsData.data' = toXyz $ y_gps_pos y
                                             , MsgGpsData.v_accuracy = 1
                                             , MsgGpsData.h_accuracy = 1
                                             , MsgGpsData.n_satellites = 0
                                             }
                         , MsgGps.velocity = MsgGpsData.GpsData
                                             { MsgGpsData.time = 0
                                             , MsgGpsData.data' = toXyz $ y_gps_vel y
                                             , MsgGpsData.v_accuracy = 1
                                             , MsgGpsData.h_accuracy = 1
                                             , MsgGpsData.n_satellites = 0
                                             }
                         }
  , Msg.airspeed = Nothing
  }


data Sensors a =
  Sensors
  { y_gyro :: V3T M a
  , y_accel :: V3T M a
  , y_gps_pos :: V3T N a
  , y_gps_vel :: V3T N a
  }


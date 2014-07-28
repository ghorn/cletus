{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# Language DoAndIfThenElse #-}
{-# Language OverloadedStrings #-}
{-# Language CPP #-}

module Main ( main ) where

import Control.Concurrent ( MVar, forkIO, modifyMVar_, newMVar, readMVar)
import Control.Monad ( forever )
import qualified Data.Foldable as F
--import System.Remote.Monitoring ( forkServer )
import qualified Text.ProtocolBuffers as PB

import SpatialMath
import Vis

import qualified Messages.Xyz as Msg
import qualified Messages.Dcm as Msg
import qualified Messages.OptTelem as Msg
import qualified Messages.AcPose as Msg

import qualified ZmqHelpers as Zmq

import DrawAC
import Channels ( chanOptTelem )

data State = State { sPlanes :: [VisObject Double]
                   , sMessages :: [String]
                   }

--toNice :: AcX Double -> (V3 Double, Quat Double, V3 Double, V3 Double, Double)
--toNice cs = (xyz, q'n'b, r'n0'a0, r'n0't0, fromMaybe 1 $ CS.visSpan cs)
--  where
--    x = KiteXyz.x $ CS.kiteXyz cs
--    y = KiteXyz.y $ CS.kiteXyz cs
--    z = KiteXyz.z $ CS.kiteXyz cs
--
--    r11 = Dcm.r11 $ CS.kiteDcm cs
--    r12 = Dcm.r12 $ CS.kiteDcm cs
--    r13 = Dcm.r13 $ CS.kiteDcm cs
--
--    r21 = Dcm.r21 $ CS.kiteDcm cs
--    r22 = Dcm.r22 $ CS.kiteDcm cs
--    r23 = Dcm.r23 $ CS.kiteDcm cs
--
--    r31 = Dcm.r31 $ CS.kiteDcm cs
--    r32 = Dcm.r32 $ CS.kiteDcm cs
--    r33 = Dcm.r33 $ CS.kiteDcm cs
--
--    delta = CS.delta cs
--
--    q'nwu'ned = Quat 0 1 0 0
--
--    q'n'a = Quat (cos(0.5*delta)) 0 0 (sin(-0.5*delta))
--
--    q'aNWU'bNWU = quatOfDcm $ fromLists [ [r11, r12, r13]
--                                        , [r21, r22, r23]
--                                        , [r31, r32, r33]
--                                        ]
--    q'a'b = q'nwu'ned * q'aNWU'bNWU * q'nwu'ned
--    q'n'b = q'n'a * q'a'b
--    q'n'aNWU = q'n'a * q'nwu'ned
--
--    rArm = V3 (CS.rArm cs) 0 0
--    xyzArm = rArm + V3 x y z
--    xyz = rotVecByQuatB2A q'n'aNWU xyzArm
--
--    zt = CS.zt cs
--    r'n0'a0 = rotVecByQuatB2A q'n'a rArm
--    r'n0't0 = xyz + (rotVecByQuatB2A q'n'b $ V3 0 0 (-zt))

fromXyz :: Msg.Xyz -> V3 Double
fromXyz (Msg.Xyz x y z) = V3 x y z

fromDcm :: Msg.Dcm -> V3 (V3 Double)
fromDcm (Msg.Dcm r0 r1 r2) = V3 (fromXyz r0) (fromXyz r1) (fromXyz r2)

drawFun :: State -> VisObject Double
----drawFun state = VisObjects $ [axes] ++ (map text [-5..5]) ++ [boxText, ac, plane,trailLines]
drawFun (State {sPlanes=planes, sMessages = messages}) =
  VisObjects $ axes :  txt : acs ++ [plane]
  where
    acs = planes

    axes = Axes (0.5, 15)
    plane = Trans (V3 0 0 planeZ) $ Plane (V3 0 0 1) (makeColor 1 1 1 1) (makeColor 0.2 0.3 0.32 (realToFrac planeAlpha))
    planeAlpha = 1 :: Int
--    planeAlpha
--      | z < planeZ' = 1
--      | z < planeZ'+2 = (planeZ'+2-z)/2
--      | otherwise = 0
    --planeZ' = planeZ-0.5

    txt = VisObjects $
          zipWith (\s k -> Text2d s (30,fromIntegral $ 30*k) TimesRoman24 (makeColor 1 1 1 1))
          messages (reverse [1..length messages])


drawOneAc :: Msg.AcPose -> VisObject Double
drawOneAc acPose = ac
  where
    pos = fromXyz (Msg.r_n2b_n acPose)
    quat = quatOfDcm (fromDcm (Msg.dcm_n2b acPose))

    visSpan = 1.5

    ac = Trans pos $ Scale (visSpan,visSpan,visSpan) ac'
    (ac',_) = drawAc kiteAlpha (V3 0 0 0) quat
--    lineAlpha = realToFrac $ fromMaybe 1 lineTransparency
    kiteAlpha = 1
--    lineTransparency = 1



planeZ :: Double
planeZ = 1

state0 :: State
state0 = State [] []

updateState :: Msg.OptTelem -> State -> IO State
updateState telem _ =
  return $ State { sMessages = map PB.toString (F.toList (Msg.messages telem))
                 , sPlanes = planes
                 }
  where
    planes = map drawOneAc $ F.toList (Msg.poses telem)
--    w0 = Msg.w0 telem
--    trails0 = sTrails x0
--    pos = fromXyz $ Msg.r_n2b_n $ Msg.state telem
--    q = quatOfDcm $ fromDcm $ Msg.dcm_n2b $ Msg.state telem
--    (_,trails) = drawAc 1 pos q


sub :: MVar State -> IO ()
sub m = Zmq.withContext $ \context ->
  Zmq.withSubscriber context chanOptTelem "opt_telem" $ \receive -> do
    forever $ do
      msg <- receive
      let cs = case Zmq.decodeProto msg of
            Left err -> error err
            Right cs' -> cs'
      modifyMVar_ m (updateState cs)
      return ()

ts :: Double
ts = 0.02

main :: IO ()
main = do
--  _ <- forkServer "localhost" 8000
  m <- newMVar state0
  _ <- forkIO (sub m)

--  threadDelay 5000000
  let simFun _ _ = return ()
      df _ = fmap drawFun (readMVar m)
  simulateIO (Just ((1260,940),(1930,40))) "baby betty sim" ts () df simFun

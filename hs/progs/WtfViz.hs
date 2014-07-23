-- Copyright 2012-2013 Greg Horn
--
-- This file is part of rawesome.
--
-- rawesome is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- rawesome is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with rawesome.  If not, see <http://www.gnu.org/licenses/>.

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# Language DoAndIfThenElse #-}
{-# Language OverloadedStrings #-}
{-# Language CPP #-}

--module Main ( main ) where

import System.Random ( Random(..), randomRs, mkStdGen)
import Control.Concurrent ( MVar, forkIO, modifyMVar_, newMVar, readMVar)
import Control.Monad ( forever )
import qualified Data.Foldable as F
--import System.Remote.Monitoring ( forkServer )
import qualified Text.ProtocolBuffers as PB

import SpatialMath
import Vis

import qualified Messages.Xyz as Msg
import qualified Messages.SimTelem as Msg
import qualified Messages.AcState as Msg
import qualified Messages.Dcm as Msg

import qualified ZmqHelpers as Zmq

import DrawAC
import Channels ( chanSimTelem )

data State = State { sTrails :: [[V3 Double]]
                   , sTelem :: Maybe Msg.SimTelem
                   , sParticles :: [V3 Double]
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
drawFun (State {sTelem=Nothing}) = VisObjects []
drawFun state@(State {sTelem=Just telem}) =
  VisObjects [axes, txt, ac, plane, trailLines, zLine, points]
  where
    cs = Msg.state telem
    pos@(V3 x y z) = fromXyz (Msg.r_n2b_n cs)
    quat = quatOfDcm (fromDcm (Msg.dcm_n2b cs))

    visSpan = 1

    points = Points (sParticles state) (Just 2) $ makeColor 1 1 1 0.5
    zLine = Line [V3 x y (planeZ-0.01), pos]            $ makeColor 0.1 0.2 1 0.5
    --xyLine = Line [V3 x y (planeZ-0.01), V3 0 0 (planeZ-0.01)] $ makeColor 0.2 0.7 1 0.5

    axes = Axes (0.5, 15)
    --arm  = Line [V3 0 0 0, r'n0'a0] $ makeColor 1 1 0 lineAlpha
    --line = Line [r'n0'a0, r'n0't0]   $ makeColor 0 1 1 lineAlpha
    plane = Trans (V3 0 0 planeZ) $ Plane (V3 0 0 1) (makeColor 1 1 1 1) (makeColor 0.2 0.3 0.32 (realToFrac planeAlpha))
    planeZ' = planeZ-0.5
    planeAlpha
      | z < planeZ' = 1
      | z < planeZ'+2 = (planeZ'+2-z)/2
      | otherwise = 0

    txt = VisObjects $
          zipWith (\s k -> Text2d s (30,fromIntegral $ 30*k) TimesRoman24 (makeColor 1 1 1 1)) messages (reverse [1..length messages])
    messages = map PB.toString (F.toList (Msg.messages telem))

    ac = Trans pos $ Scale (visSpan,visSpan,visSpan) ac'
    (ac',_) = drawAc kiteAlpha (V3 0 0 0) quat
--    lineAlpha = realToFrac $ fromMaybe 1 lineTransparency
    kiteAlpha = 1
--    lineTransparency = 1

    trailLines = drawTrails (sTrails state)

planeZ :: Double
planeZ = 1

particleBox :: Double
particleBox = 8

instance Random a => Random (V3 a) where
  random g0 = (V3 x y z, g3)
    where
      (x, g1) = random g0
      (y, g2) = random g1
      (z, g3) = random g2
  randomR (V3 lbx lby lbz, V3 ubx uby ubz) g0 = (V3 x y z, g3)
    where
      (x, g1) = randomR (lbx, ubx) g0
      (y, g2) = randomR (lby, uby) g1
      (z, g3) = randomR (lbz, ubz) g2

state0 :: State
state0 = State { sTelem = Nothing
               , sTrails = [[],[],[]]
               , sParticles = take 300 $ randomRs (V3 (-particleBox) (-particleBox) (planeZ-2*particleBox),
                                                   V3 particleBox particleBox planeZ)
                              (mkStdGen 0)
               }

updateTrail :: [V3 a] -> V3 a -> [V3 a]
updateTrail trail0 xyz
  | length trail0 < 65 = xyz:trail0
  | otherwise = take 65 (xyz:trail0)

boundParticle :: V3 Double -> V3 Double
boundParticle xyz@(V3 x y z)
  | x >  particleBox = boundParticle (V3 (x-2*particleBox) y z)
  | x < -particleBox = boundParticle (V3 (x+2*particleBox) y z)
  | y >  particleBox = boundParticle (V3 x (y-2*particleBox) z)
  | y < -particleBox = boundParticle (V3 x (y+2*particleBox) z)
  | z > planeZ               = boundParticle (V3 x y (z-2*particleBox))
  | z < planeZ-2*particleBox = boundParticle (V3 x y (z+2*particleBox))
  | otherwise = xyz

windShear :: Double -> Double -> Double
windShear w0 z
  | z' < zt = 0
  | otherwise = w0*log(z'/zt)/log(z0/zt)
  where
    z' = z + planeZ + zt + 2
    z0 = 100
    zt = 0.1

updateState :: Msg.SimTelem -> State -> IO State
updateState telem x0 =
  return $ State { sTelem = Just telem
                 , sTrails = zipWith updateTrail trails0 trails
                 , sParticles = map (\xyz@(V3 _ _ z) -> boundParticle $ (V3 (ts*(windShear w0 (-z))) 0 0) + xyz) (sParticles x0)
                 }
  where
    w0 = Msg.w0 telem
    trails0 = sTrails x0
    pos = fromXyz $ Msg.r_n2b_n $ Msg.state telem
    q = quatOfDcm $ fromDcm $ Msg.dcm_n2b $ Msg.state telem
    (_,trails) = drawAc 1 pos q


sub :: MVar State -> IO ()
sub m = Zmq.withContext $ \context ->
  Zmq.withSubscriber context chanSimTelem "sim_telemetry" $ \receive -> do
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

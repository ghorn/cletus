{-# OPTIONS_GHC -Wall #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveFunctor #-}
{-# Language ScopedTypeVariables #-}

module Main ( main ) where

import GHC.Generics
import Linear
import SpatialMath ( dcmOfQuat )
import qualified Data.Vector as V
import qualified Data.Text as T

import Dyno.Vectorize
import Dyno.Casadi.SX
import Dyno.Casadi.SXElement
import qualified Casadi.Core.Tools as C

data Xe a =
  Xe
  { xe'r_n2m_n :: V3 a
  , xe'v_mn_n :: V3 a
  , xe'err_m2e :: V3 a
  , xe'gyroBias :: V3 a
  , xe'accelBias :: V3 a
  } deriving (Functor, Generic, Generic1, Show)

data Imu a =
  Imu
  { imu'gyros :: V3 a
  , imu'accels :: V3 a
  } deriving (Functor, Generic, Generic1, Show)

data W a =
  W
  { w'gyros :: V3 a
  , w'accels :: V3 a
  , w'gyroDrift :: V3 a
  , w'accelDrift :: V3 a
  } deriving (Functor, Generic, Generic1, Show)
data Consts a =
  Consts
  { c'q_n2m :: Quaternion a
  , c'tauGyros :: a
  , c'tauAccels :: a
  , c'g :: a
  , c'dt :: a
  , c'r_m2a_m :: V3 a
  } deriving (Functor, Generic, Generic1, Show)
data H a =
  H
  { h'r_n2a_n :: V3 a
  , h'v_an_n :: V3 a
  } deriving (Functor, Generic, Generic1, Show)

data FunInputs a = FunInputs (Xe a) (Imu a) (Consts a) deriving (Functor, Generic, Generic1, Show)
instance Vectorize Xe
instance Vectorize W
instance Vectorize Consts
instance Vectorize Imu
instance Vectorize H
instance Vectorize FunInputs

quatMult :: Num a => Quaternion a -> Quaternion a -> Quaternion a
quatMult (Quaternion s1 v1) (Quaternion s2 v2) =
  Quaternion (s1*s2 - (v1 `dot` v2)) $
  (v1 `cross` v2) + s1*^v2 + s2*^v1

--ddt :: Fractional a => Consts a -> Xe a -> Imu a -> Xe a
--ddt c (Xe _ v_mn_n err_m2e gyroBias accelBias) (Imu gyros accels) =
--  Xe
--  { xe'r_n2m_n = v_mn_n
--  , xe'v_mn_n = a_mn_n
--  , xe'err_m2e = 0.5 *^ w_mn_m
--  , xe'gyroBias = -gyroBias ^/(c'tauGyros c)
--  , xe'accelBias = -accelBias ^/(c'tauAccels c)
--  }
--  where
--    w_mn_m = gyros - gyroBias
--
--    q_m2e = Quaternion 1 err_m2e
--    q_n2e = (c'q_n2m c) `quatMult` q_m2e
--
--    dcm_n2e = dcmOfQuat q_n2e
--
--    -- left multiply - no conjugate needed
--    a_mn_n = (accels *! dcm_n2e) - (V3 0 0 (c'g c))

propogate :: Floating a => Consts a -> Xe a -> W a -> Imu a -> Xe a
propogate c
  (Xe r_n2m_n v_mn_n err_m2e gyroBias accelBias)
  (W wGyro wAccel wGyroDrift wAccelDrift)
  (Imu gyros accels) =
  Xe
  { xe'r_n2m_n = r_n2m_n + dt *^ v_mn_n + (dt*dt*0.5) *^ a_mn_n
  , xe'v_mn_n = v_mn_n + dt *^ a_mn_n
  , xe'err_m2e = err_m2e + (0.5*dt) *^ w_mn_m
  , xe'gyroBias  =  gyroBias ^* exp (-dt / (c'tauGyros c)) + wGyroDrift
  , xe'accelBias = accelBias ^* exp (-dt / (c'tauAccels c)) + wAccelDrift
  }
  where
    dt = c'dt c
    
    w_mn_m = gyros + wGyro - gyroBias

    q_m2e = Quaternion 1 err_m2e
    q_n2e = (c'q_n2m c) `quatMult` q_m2e

    dcm_n2e = dcmOfQuat q_n2e

    -- left multiply - no conjugate needed
    a_mn_n = ((accels + wAccel - accelBias) *! dcm_n2e) - (V3 0 0 (c'g c))

gpsH :: Floating a => Consts a -> Imu a -> Xe a -> H a
gpsH c imu (Xe r_n2m_n v_mn_n err_m2e gyroBias _) =
  H
  { h'r_n2a_n = r_n2m_n + r_m2a_n
  , h'v_an_n = v_mn_n + w_mn_m `cross` r_m2a_m
  }
  where
    gyros = imu'gyros imu
    w_mn_m = gyros - gyroBias
    r_m2a_m = c'r_m2a_m c
    -- left multiply - no conjugate needed
    r_m2a_n = r_m2a_m *! dcm_n2e

    q_m2e = Quaternion 1 err_m2e
    q_n2e = (c'q_n2m c) `quatMult` q_m2e

    dcm_n2e = dcmOfQuat q_n2e

casadiSsyms :: String -> Int -> IO (V.Vector SXElement)
casadiSsyms name k = fmap V.fromList $ mapM (sxElement_sym . (name ++) . show) (take k [(0::Int)..])

sym :: forall f . Vectorize f => String -> IO (f SXElement)
sym name = do
  let n = vlength (Proxy :: Proxy f)
  x <- casadiSsyms name n
  return (devectorize x)

--lstrip :: String -> String
--lstrip (' ':xs) = lstrip xs
--lstrip xs = xs
--
--rstrip :: String -> String
--rstrip = reverse . lstrip . reverse
--
--strip :: String -> String
--strip = rstrip . lstrip

sketchyPrint :: String -> SX -> String
sketchyPrint varname sx' = init $ unlines $ map ((++ ";") . (("    " ++ varname) ++) . formatLine) $ tail $ lines $ show sx'
  where
    formatLine ln = T.unpack $ T.replace (T.pack "->") (T.pack "=") $ T.strip $ T.pack ln

main :: IO ()
main = do
  r_m2n_n <- sym "r_m2n_n_"
  v_mn_n <- sym "v_mn_n_"
  err_m2e <- sym "e"
  gyroBias <- sym "gb"
  accelBias <- sym "ab"
  let x0 = Xe r_m2n_n v_mn_n err_m2e gyroBias accelBias

  
  wgyro <- sym "wg"
  waccel <- sym "wa"
  wgyroDrift <- sym "wgd"
  waccelDrift <- sym "wad"
  let noise = W wgyro waccel wgyroDrift waccelDrift

  gyro <- sym "gyro"
  accel <- sym "acc"
  let imu = Imu gyro accel

  r_m2a_m <- sym "r_m2a_m_"
  q_n2m <- sym "q"
  g <- sxElement_sym "g"
  dt <- sxElement_sym "dt"
  tauGyros <- sxElement_sym "tauG"
  tauAccel <- sxElement_sym "tauA"
  let c = Consts { c'q_n2m = q_n2m
                 , c'tauGyros = tauGyros
                 , c'tauAccels = tauAccel
                 , c'g = g
                 , c'dt = dt
                 , c'r_m2a_m = r_m2a_m
                 }
  let xnext@(Xe r v err gb ab) = propogate c x0 noise imu
      h = gpsH c imu x0

  putStrLn "\nx0"
  print x0
  putStrLn "\nr"
  print r
  putStrLn "\nv"
  print v
  putStrLn "\nerr"
  print err
  putStrLn "\ngb"
  print gb
  putStrLn "\nab"
  print ab

  let xnextV = svector $ vectorize xnext
      jacF' = sjacobian xnextV (svector (vectorize x0))
      jacG' = sjacobian xnextV (svector (vectorize noise))
      jacH' = sjacobian (svector (vectorize h)) (svector (vectorize x0))

      zero' :: Vectorize f => f SXElement -> [(SXElement, SXElement)]
      zero' f = map (\x -> (x,0)) (V.toList (vectorize f))
      
      subs = zero' err_m2e ++ zero' wgyro ++ zero' waccel ++ zero' wgyroDrift ++ zero' waccelDrift
--             [ (exp((-(dt/tauGyros))), 0)
--             ]
      (sub0,subF) = V.unzip (V.fromList subs)
  jacF <- C.substitute__3 jacF' (svector sub0) (svector subF)
  jacG <- C.substitute__3 jacG' (svector sub0) (svector subF)
  jacH <- C.substitute__3 jacH' (svector sub0) (svector subF)
  putStrLn "\nF:"
  putStrLn (sketchyPrint "F" jacF)
  putStrLn "\nG:"
  putStrLn (sketchyPrint "G" jacG)
  putStrLn "\nH:"
  putStrLn (sketchyPrint "H" jacH)

{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}

module Aircraft ( SimTelem(..), AcX(..), AcU(..), aircraftDae, aircraftOde
                ) where

import GHC.Generics
import Control.Applicative
import Linear
import Data.Serialize

import Dyno.Vectorize
import Dyno.Server.Accessors ( Lookup(..) )

import AeroCoeffs

data AcX a = AcX { ac_r_n2b_n :: V3 a
                 , ac_v_bn_b :: V3 a
                 , ac_R_n2b :: M33 a
                 , ac_w_bn_b :: V3 a
                 } deriving (Eq, Functor, Generic, Generic1, Show)
data AcU a = AcU { acSurfaces :: ControlSurfaces a
                 } deriving (Eq, Functor, Generic, Generic1, Show)
data SimTelem =
  SimTelem
  { stX :: AcX Double
  , stU :: AcU Double
  , stMessages :: [String]
  , stW0 :: Double
  } deriving (Generic, Show)
instance Serialize SimTelem
instance Applicative AcX where
  pure = fill
  AcX x0 y0 (V3 z00 z01 z02) w0 <*> AcX x1 y1 (V3 z10 z11 z12) w1 =
    AcX
    (x0 <*> x1)
    (y0 <*> y1)
    (V3 (z00 <*> z10) (z01 <*> z11) (z02 <*> z12))
    (w0 <*> w1)
  
newtype AcZ a = AcZ (None a) deriving (Eq, Functor, Generic, Generic1, Show)
newtype AcR a = AcR (AcX a) deriving (Eq, Functor, Generic, Generic1, Show)
newtype AcP a = AcP (None a) deriving (Eq, Functor, Generic, Generic1, Show)

instance Serialize a => Serialize (V3 a)
instance Serialize a => Serialize (AcX a)
instance Serialize a => Serialize (AcU a)
instance Vectorize AcX
instance Vectorize AcZ
instance Vectorize AcU
instance Vectorize AcP
instance Vectorize AcR
instance Additive AcX where
  zero = fill 0

instance (Lookup a, Generic a) => Lookup (AcX a)
instance (Lookup a, Generic a) => Lookup (AcZ a)
instance (Lookup a, Generic a) => Lookup (AcU a)
instance (Lookup a, Generic a) => Lookup (AcP a)
instance (Lookup a, Generic a) => Lookup (AcR a)

aircraftDae :: forall a. Floating a =>
       (a, M33 a) -> AeroForceCoeffs a -> AeroMomentCoeffs a -> AeroRefs a ->
       AcX a -> AcX a -> AcU a -> AcX a
aircraftDae
  (mass, inertia)
  forceCoeffs
  momentCoeffs
  refs
  (AcX r_n2b_n' v_bn_b' dcm_n2b' w_bn_b')
  (AcX       _  v_bn_b  dcm_n2b  w_bn_b )
  (AcU controlSurfaces) = daeResidual
  where
    v_bw_b = v_bn_b -- no relative wind
    (aero_forces_body, moments_body) = aeroForcesMoments forceCoeffs momentCoeffs refs v_bw_b w_bn_b controlSurfaces
    forces_body = aero_forces_body + dcm_n2b !* (V3 0 0 (9.81*mass))

    daeResidual =
      AcX { ac_r_n2b_n = (trans dcm_n2b) !* v_bn_b - r_n2b_n'
          , ac_v_bn_b = v_bn_b' + cross w_bn_b v_bn_b - forces_body ^/ mass
          , ac_R_n2b = (trans (skew w_bn_b)) !*! dcm_n2b - dcm_n2b'
          , ac_w_bn_b = inertia !* w_bn_b' + cross w_bn_b (inertia !* w_bn_b) - moments_body
          }

aircraftOde :: forall a. Floating a =>
       (a, M33 a) -> AeroForceCoeffs a -> AeroMomentCoeffs a -> AeroRefs a ->
       AcX a -> AcU a -> AcX a
aircraftOde
  (mass, inertia)
  forceCoeffs
  momentCoeffs
  refs
  (AcX       _  v_bn_b  dcm_n2b  w_bn_b )
  (AcU controlSurfaces) = daeResidual
  where
    v_bw_b = v_bn_b -- no relative wind
    (aero_forces_body, moments_body) = aeroForcesMoments forceCoeffs momentCoeffs refs v_bw_b w_bn_b controlSurfaces
    forces_body = aero_forces_body + dcm_n2b !* (V3 0 0 (9.81*mass))

    daeResidual =
      AcX { ac_r_n2b_n = (trans dcm_n2b) !* v_bn_b
          , ac_v_bn_b = forces_body ^/ mass - cross w_bn_b v_bn_b
          , ac_R_n2b = (trans (skew w_bn_b)) !*! dcm_n2b
          , ac_w_bn_b = inertiaInv !* (moments_body - cross w_bn_b (inertia !* w_bn_b))
          }
    inertiaInv = myInv33 inertia

myInv33 :: Fractional a => M33 a -> M33 a
myInv33 m@(V3 (V3 a b c)
              (V3 d e f)
              (V3 g h i))
  | otherwise = (1 / det) *!! V3 (V3 a' b' c')
                                 (V3 d' e' f')
                                 (V3 g' h' i')
  where a' = cofactor (e,f,h,i)
        b' = cofactor (c,b,i,h)
        c' = cofactor (b,c,e,f)
        d' = cofactor (f,d,i,g)
        e' = cofactor (a,c,g,i)
        f' = cofactor (c,a,f,d)
        g' = cofactor (d,e,g,h)
        h' = cofactor (b,a,h,g)
        i' = cofactor (a,b,d,e)
        cofactor (q,r,s,t) = det22 (V2 (V2 q r) (V2 s t))
        det = det33 m

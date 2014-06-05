{-# OPTIONS_GHC -Wall #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FunctionalDependencies #-}
{-# Language FlexibleInstances #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveFoldable #-}
{-# Language DeriveTraversable #-}
{-# Language DeriveGeneric #-}

module SpatialMathT ( Rotation(..)
                    , Rot(..)
                    , V3T(..)
                    , M33T
                    , cross
                    ) where

import Control.Applicative ( Applicative )
import Data.Foldable ( Foldable )
import Data.Serialize
import Data.Traversable ( Traversable )
import Foreign.Storable ( Storable )
import GHC.Generics

import Linear hiding ( cross )
import qualified Linear as L

import SpatialMath
import Dyno.Vectorize
import Dyno.Server.Accessors ( Lookup(..) )

newtype V3T f a = V3T {unV :: V3 a}
                deriving ( Functor, Foldable, Traversable
                         , Applicative
                         , Additive, Storable
                         , Num, Fractional, Eq, Show
                         , Generic1, Generic
                         )
instance Vectorize (V3T f)
instance Vectorize (Rot f1 f2)
instance (Lookup a, Generic a) => Lookup (Rot f1 f2 a)
instance (Lookup a, Generic a) => Lookup (V3T f a)

instance Serialize a => Serialize (V3T f a) where
  get = do
    x <- get
    y <- get
    z <- get
    return (V3T (V3 x y z))
  put (V3T (V3 x y z)) = do
    put x
    put y
    put z

cross :: Num a => V3T f a -> V3T f a -> V3T f a
cross (V3T vx) (V3T vy) = V3T (vx `L.cross` vy)

newtype Rot f1 f2 r = Rot { unR :: r }
                    deriving ( Functor, Foldable, Traversable
                             , Storable
                             , Num, Fractional, Eq, Show, Serialize
                             , Generic1, Generic
                             )

type M33T f1 f2 a = V3T f1 (V3T f2 a)

class Rotation p a | p -> a where
  compose :: Rot f1 f2 p -> Rot f2 f3 p -> Rot f1 f3 p
  rot  :: Rot f1 f2 p -> V3T f1 a -> V3T f2 a
  rot' :: Rot f1 f2 p -> V3T f2 a -> V3T f1 a
  toDcm   :: Rot f1 f2 p -> Rot f1 f2 (M33 a)
--  fromDcm :: Rot f1 f2 (M33 a) -> Rot f1 f2 (p a)
  transpose :: Rot f1 f2 p -> Rot f2 f1 p

instance Num a => Rotation (Quaternion a) a where
  compose (Rot q_a2b) (Rot q_b2c) = Rot (q_a2b `quatMult` q_b2c)
  rot  (Rot q_a2b) (V3T va) = V3T (rotVecByQuat    q_a2b va)
  rot' (Rot q_a2b) (V3T vb) = V3T (rotVecByQuatB2A q_a2b vb)
  toDcm (Rot q_a2b) = Rot (dcmOfQuat q_a2b)
--  fromDcm (Rot dcm_a2b) = Rot (quatOfDcm dcm_a2b)
  transpose (Rot (Quaternion q0 qxyz)) = Rot (Quaternion q0 (fmap negate qxyz))

-- quaternion multiplication which doesn't require RealFrac
quatMult :: Num a => Quaternion a -> Quaternion a -> Quaternion a
quatMult (Quaternion s1 v1) (Quaternion s2 v2) =
  Quaternion (s1*s2 - (v1 `dot` v2)) $
  (v1 `L.cross` v2) + s1*^v2 + s2*^v1

instance Num a => Rotation (M33 a) a where
  compose (Rot dcm_a2b) (Rot dcm_b2c) = Rot (dcm_b2c !*! dcm_a2b)
  rot  (Rot dcm_a2b) (V3T va) = V3T (rotVecByDcm    dcm_a2b va)
  rot' (Rot dcm_a2b) (V3T vb) = V3T (rotVecByDcmB2A dcm_a2b vb)
  toDcm = id
  transpose (Rot (V3
                  (V3 e11 e12 e13)
                  (V3 e21 e22 e23)
                  (V3 e31 e32 e33))) =
    Rot (V3
         (V3 e11 e21 e31)
         (V3 e12 e22 e32)
         (V3 e13 e23 e33))

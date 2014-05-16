{-# LINE 1 "sim/src/Structs/Structures.hsc" #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LINE 2 "sim/src/Structs/Structures.hsc" #-}

{-# LINE 3 "sim/src/Structs/Structures.hsc" #-}

{-# LINE 4 "sim/src/Structs/Structures.hsc" #-}
module Structs.Structures where
import Foreign.Ptr
import Foreign.Ptr (Ptr,FunPtr,plusPtr)
import Foreign.Ptr (wordPtrToPtr,castPtrToFunPtr)
import Foreign.Storable
import Foreign.C.Types
import Foreign.C.String (CString,CStringLen,CWString,CWStringLen)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray,pokeArray)
import Data.Int
import Data.Word

{-# LINE 7 "sim/src/Structs/Structures.hsc" #-}

{- typedef struct {
            uint64_t tsec; uint64_t tnsec;
        } timestamp_t; -}

{-# LINE 12 "sim/src/Structs/Structures.hsc" #-}

{-# LINE 13 "sim/src/Structs/Structures.hsc" #-}

{-# LINE 14 "sim/src/Structs/Structures.hsc" #-}
data C'timestamp_t = C'timestamp_t{
  c'timestamp_t'tsec :: CULong,
  c'timestamp_t'tnsec :: CULong
} deriving (Eq,Show)
p'timestamp_t'tsec p = plusPtr p 0
p'timestamp_t'tsec :: Ptr (C'timestamp_t) -> Ptr (CULong)
p'timestamp_t'tnsec p = plusPtr p 8
p'timestamp_t'tnsec :: Ptr (C'timestamp_t) -> Ptr (CULong)
instance Storable C'timestamp_t where
  sizeOf _ = 16
  alignment _ = 8
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 8
    return $ C'timestamp_t v0 v1
  poke p (C'timestamp_t v0 v1) = do
    pokeByteOff p 0 v0
    pokeByteOff p 8 v1
    return ()

{-# LINE 15 "sim/src/Structs/Structures.hsc" #-}
{- typedef struct {
            double x; double y; double z;
        } xyz_t; -}

{-# LINE 19 "sim/src/Structs/Structures.hsc" #-}

{-# LINE 20 "sim/src/Structs/Structures.hsc" #-}

{-# LINE 21 "sim/src/Structs/Structures.hsc" #-}

{-# LINE 22 "sim/src/Structs/Structures.hsc" #-}
data C'xyz_t = C'xyz_t{
  c'xyz_t'x :: CDouble,
  c'xyz_t'y :: CDouble,
  c'xyz_t'z :: CDouble
} deriving (Eq,Show)
p'xyz_t'x p = plusPtr p 0
p'xyz_t'x :: Ptr (C'xyz_t) -> Ptr (CDouble)
p'xyz_t'y p = plusPtr p 8
p'xyz_t'y :: Ptr (C'xyz_t) -> Ptr (CDouble)
p'xyz_t'z p = plusPtr p 16
p'xyz_t'z :: Ptr (C'xyz_t) -> Ptr (CDouble)
instance Storable C'xyz_t where
  sizeOf _ = 24
  alignment _ = 8
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 8
    v2 <- peekByteOff p 16
    return $ C'xyz_t v0 v1 v2
  poke p (C'xyz_t v0 v1 v2) = do
    pokeByteOff p 0 v0
    pokeByteOff p 8 v1
    pokeByteOff p 16 v2
    return ()

{-# LINE 23 "sim/src/Structs/Structures.hsc" #-}
{- typedef struct {
            timestamp_t timestamp;
            xyz_t gyro;
            xyz_t accel;
            xyz_t gps_pos;
            xyz_t gps_vel;
        } sensors_t; -}

{-# LINE 31 "sim/src/Structs/Structures.hsc" #-}

{-# LINE 32 "sim/src/Structs/Structures.hsc" #-}

{-# LINE 33 "sim/src/Structs/Structures.hsc" #-}

{-# LINE 34 "sim/src/Structs/Structures.hsc" #-}

{-# LINE 35 "sim/src/Structs/Structures.hsc" #-}

{-# LINE 36 "sim/src/Structs/Structures.hsc" #-}
data C'sensors_t = C'sensors_t{
  c'sensors_t'timestamp :: C'timestamp_t,
  c'sensors_t'gyro :: C'xyz_t,
  c'sensors_t'accel :: C'xyz_t,
  c'sensors_t'gps_pos :: C'xyz_t,
  c'sensors_t'gps_vel :: C'xyz_t
} deriving (Eq,Show)
p'sensors_t'timestamp p = plusPtr p 0
p'sensors_t'timestamp :: Ptr (C'sensors_t) -> Ptr (C'timestamp_t)
p'sensors_t'gyro p = plusPtr p 16
p'sensors_t'gyro :: Ptr (C'sensors_t) -> Ptr (C'xyz_t)
p'sensors_t'accel p = plusPtr p 40
p'sensors_t'accel :: Ptr (C'sensors_t) -> Ptr (C'xyz_t)
p'sensors_t'gps_pos p = plusPtr p 64
p'sensors_t'gps_pos :: Ptr (C'sensors_t) -> Ptr (C'xyz_t)
p'sensors_t'gps_vel p = plusPtr p 88
p'sensors_t'gps_vel :: Ptr (C'sensors_t) -> Ptr (C'xyz_t)
instance Storable C'sensors_t where
  sizeOf _ = 112
  alignment _ = 8
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 16
    v2 <- peekByteOff p 40
    v3 <- peekByteOff p 64
    v4 <- peekByteOff p 88
    return $ C'sensors_t v0 v1 v2 v3 v4
  poke p (C'sensors_t v0 v1 v2 v3 v4) = do
    pokeByteOff p 0 v0
    pokeByteOff p 16 v1
    pokeByteOff p 40 v2
    pokeByteOff p 64 v3
    pokeByteOff p 88 v4
    return ()

{-# LINE 37 "sim/src/Structs/Structures.hsc" #-}
{- typedef struct {
            timestamp_t start;
            timestamp_t stop;
            double flaps;
            double ail;
            double rudd;
            double elev;
        } actuators_t; -}

{-# LINE 46 "sim/src/Structs/Structures.hsc" #-}

{-# LINE 47 "sim/src/Structs/Structures.hsc" #-}

{-# LINE 48 "sim/src/Structs/Structures.hsc" #-}

{-# LINE 49 "sim/src/Structs/Structures.hsc" #-}

{-# LINE 50 "sim/src/Structs/Structures.hsc" #-}

{-# LINE 51 "sim/src/Structs/Structures.hsc" #-}

{-# LINE 52 "sim/src/Structs/Structures.hsc" #-}
data C'actuators_t = C'actuators_t{
  c'actuators_t'start :: C'timestamp_t,
  c'actuators_t'stop :: C'timestamp_t,
  c'actuators_t'flaps :: CDouble,
  c'actuators_t'ail :: CDouble,
  c'actuators_t'rudd :: CDouble,
  c'actuators_t'elev :: CDouble
} deriving (Eq,Show)
p'actuators_t'start p = plusPtr p 0
p'actuators_t'start :: Ptr (C'actuators_t) -> Ptr (C'timestamp_t)
p'actuators_t'stop p = plusPtr p 16
p'actuators_t'stop :: Ptr (C'actuators_t) -> Ptr (C'timestamp_t)
p'actuators_t'flaps p = plusPtr p 32
p'actuators_t'flaps :: Ptr (C'actuators_t) -> Ptr (CDouble)
p'actuators_t'ail p = plusPtr p 40
p'actuators_t'ail :: Ptr (C'actuators_t) -> Ptr (CDouble)
p'actuators_t'rudd p = plusPtr p 48
p'actuators_t'rudd :: Ptr (C'actuators_t) -> Ptr (CDouble)
p'actuators_t'elev p = plusPtr p 56
p'actuators_t'elev :: Ptr (C'actuators_t) -> Ptr (CDouble)
instance Storable C'actuators_t where
  sizeOf _ = 64
  alignment _ = 8
  peek p = do
    v0 <- peekByteOff p 0
    v1 <- peekByteOff p 16
    v2 <- peekByteOff p 32
    v3 <- peekByteOff p 40
    v4 <- peekByteOff p 48
    v5 <- peekByteOff p 56
    return $ C'actuators_t v0 v1 v2 v3 v4 v5
  poke p (C'actuators_t v0 v1 v2 v3 v4 v5) = do
    pokeByteOff p 0 v0
    pokeByteOff p 16 v1
    pokeByteOff p 32 v2
    pokeByteOff p 40 v3
    pokeByteOff p 48 v4
    pokeByteOff p 56 v5
    return ()

{-# LINE 53 "sim/src/Structs/Structures.hsc" #-}

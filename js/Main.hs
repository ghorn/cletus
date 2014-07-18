{-# OPTIONS_GHC -Wall #-}
{-# Language ScopedTypeVariables #-}

module Main ( main ) where

-- from base:
import System.IO
import System.Exit
import Data.Functor
import Data.List
import Control.Monad
import GHC.Word ( Word8 )
import Text.Printf ( printf )

import qualified Data.List.NonEmpty as NE
import qualified System.ZMQ4 as ZMQ

import qualified Data.ByteString as B ( unpack )
import Data.ByteString.Char8 ( pack )
import           Data.Vector      ( (!) )
import qualified Data.Vector as V ( toList )
import qualified System.USB as USB

runme :: ZMQ.Sender a => USB.Device -> USB.DeviceHandle -> ZMQ.Socket a -> IO ()
runme dev devHndl rcPublisher = do
  -- Inspecting descriptors:
  config0 <- USB.getConfigDesc dev 0
  let interface0 = USB.configInterfaces config0 ! 0
      alternate0 = interface0 ! 0
      endpoint1  = USB.interfaceEndpoints alternate0 ! 0
      mps        = USB.maxPacketSize $ USB.endpointMaxPacketSize endpoint1
  
      nrOfBytesToRead = mps
  
      timeout = 5000
  
      readCycle :: Maybe Word8 -> Int -> IO ()
      readCycle _ 0 = return ()
      readCycle lastK7 k = do
        -- Performing I/O:
        (bs, status) <- USB.readInterrupt
                        devHndl
                        (USB.endpointAddress endpoint1)
                        nrOfBytesToRead
                        timeout
  
        when (status == USB.TimedOut) $ putStrLn "Reading timed out!"
        let ks = B.unpack bs
        case lastK7 of Nothing -> return ()
                       Just k7' -> do
                         let js = listToJs k7' ks
                         print js
                         ZMQ.sendMulti rcPublisher
                           (NE.fromList [pack "rc"]) -- , encode js])
        readCycle (Just (ks !! 7)) (k-1)
  readCycle Nothing 100000

main :: IO ()
main =
  -- zeromq setup
  ZMQ.withContext $ \context ->
    ZMQ.withSocket context ZMQ.Pub $ \rcPublisher -> do
      ZMQ.bind rcPublisher "ipc:///tmp/rc"
    
      -- usb setup
      ctx <- USB.newCtx
      USB.setDebug ctx USB.PrintInfo
    
      -- Enumerating devices & finding the right device:
      devs <- V.toList <$> USB.getDevices ctx
      deviceDescs <- mapM USB.getDeviceDesc devs
      case fmap fst $ find (isMyThingy . snd) $ zip devs deviceDescs of
        Nothing -> hPutStrLn stderr "Joystick not found" >> exitFailure
        Just dev -> do
          -- Opening the device:
          USB.withDeviceHandle dev $ \devHndl ->
            USB.withDetachedKernelDriver devHndl 0 $
              USB.withClaimedInterface devHndl 0 $ do
                runme dev devHndl rcPublisher

range :: forall a b . (Bounded a, Real a, Fractional b) => a -> (b,b) -> b
range x (miny, maxy) = miny + (maxy - miny)*(realToFrac x - minx)/(maxx - minx)
  where
    minx = realToFrac (minBound :: a)
    maxx = realToFrac (maxBound :: a)

toMode :: forall a . (Bounded a, Num a, Integral a) => a -> Mode
toMode x
  | x < oneQuarter = Mode0
  | x > threeQuarters = Mode2
  | otherwise = Mode1
  where
    minx = minBound :: a
    maxx = maxBound :: a

    oneQuarter    = minx +    (maxx - minx) `div` 4
    threeQuarters = minx + 3*((maxx - minx) `div` 4)

toUpDown :: forall a . (Bounded a, Num a, Integral a) => a -> UpDown
toUpDown x
  | x < half = Down
  | otherwise = Up
  where
    minx = minBound :: a
    maxx = maxBound :: a

    half    = minx +    (maxx - minx) `div` 2

data Mode = Mode0 | Mode1 | Mode2 deriving (Show, Eq, Ord, Enum)
data UpDown = Up | Down deriving (Show, Eq, Ord, Enum)
data Js =
  Js
  { jsThrottle :: Double
  , jsYaw :: Double
  , jsPitch :: Double
  , jsRoll :: Double
  , jsGear :: UpDown
  , jsAux2 :: Mode
  , jsRightTrim :: Double
  , jsFmode :: Mode
  }

instance Show Js where
  show js =
    printf
    "Js {throttle: %.3f, yaw: % .3f, pitch: % .3f, roll: % .3f, gear: %4s, aux2: %s, rightTrim % .3f, fmode %s}"
    (jsThrottle js) (jsYaw js) (jsPitch js) (jsRoll js)
    (show (jsGear js)) (show (jsAux2 js)) (jsRightTrim js) (show (jsFmode js))

newtype PWord8 = PWord8 Word8
instance Show PWord8 where
  show (PWord8 k) = case length (show k) of
    1 -> " 00" ++ show k
    2 -> " 0" ++ show k
    3 -> " " ++ show k
    _ -> error "DAMMIT"

listToJs :: Word8 -> [Word8] -> Js
listToJs k7' [k0,k1,k2,k3,k4,k5,k6,k7]
  | k1 /= 0 = error $ "got non-zero channel 1: " ++ show k1
  | otherwise =
    Js { jsThrottle = range k0 (0,1)
       , jsYaw = range k4 (1,-1)
       , jsPitch = range k3 (1,-1)
       , jsRoll = range k2 (1,-1)
       , jsGear = toUpDown k5
       , jsAux2 = toMode k6
       , jsRightTrim = range rt (-1,1)
       , jsFmode = toMode fm
       }
    where
      (rt,fm) = case (even k7', even k7) of
        (False, True) -> (k7', k7)
        (True, False) -> (k7, k7')
        other -> error $ "listToJs got " ++ show other
listToJs _ ks = error $ "length should be 8, but got " ++ show (length ks)

isMyThingy :: USB.DeviceDesc -> Bool
isMyThingy devDesc =  USB.deviceVendorId  devDesc == 0x1781
                   && USB.deviceProductId devDesc == 0x0898

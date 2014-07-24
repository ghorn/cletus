{-# OPTIONS_GHC -Wall #-}
--{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language RankNTypes #-}

module Main ( main ) where

import Linear
import Data.Vector ( Vector )
import GHC.Generics ( Generic )
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Sequence as S
import qualified Text.ProtocolBuffers as PB
import Text.Printf ( printf )

import Dyno.Vectorize
import Dyno.View
import Dyno.Ipopt
import Dyno.Snopt
import Dyno.Nlp
import Dyno.NlpSolver ( NlpSolverStuff, solveNlp' )

import Dyno.Ocp
import Dyno.DirectCollocation
import Dyno.DirectCollocation.Types ( CollStage(..), CollPoint(..) )
import Dyno.DirectCollocation.Formulate ( makeGuess )
import Dyno.DirectCollocation.Dynamic
import Dyno.Cov
import Dyno.Nats

import SpatialMathT
import MsgHelpers

import Model.Aircraft
import Model.AeroCoeffs
import Model.Betty

import qualified Messages.AcPose as Msg
import qualified Messages.OptTelem as Msg

import Channels

import qualified ZmqHelpers as Zmq

type NCollStages = D80
type CollDeg = D3

data PathC a  = PathC { pcAlphaDeg :: a
                      , pcBetaDeg :: a
                      , pcAirspeed :: a
                      } deriving (Eq, Functor, Generic, Generic1, Show)
data Bc0 a  = Bc0  (AcX a)        deriving (Eq, Functor, Generic, Generic1, Show)
data Bc12 a = Bc12 (AcX a) (V3T N a) deriving (Eq, Functor, Generic, Generic1, Show)
instance Vectorize PathC
instance Vectorize Bc0
instance Vectorize Bc12

--maxTime :: (Floating a) => a
--maxTime = 300

--scalProd ::(Num a) => V3 a -> V3 a -> a
--scalProd (V3 x y z) (V3 x' y' z') = x*x' + y*y' -- + z*z'

--getvX :: V3 a -> a
--getvX (V3 x _ _) = x

--getvY :: V3 a -> a
--getvY (V3 _ x _) = x

--getvZ :: V3 a -> a
--getvZ (V3 _ _ x) = x

lagrange :: Floating a => AcX a -> None a -> AcU a -> None a -> AeroOutputs a -> a -> a
lagrange (AcX _pos _vit _ _) _ (AcU surfs) _ _ _ = 0
-- + (scalProd pos pos)-- / sqrt (scalProd pos pos)
 + (elev**2 + rudd**2 + ail**2 + flaps**2)
-- + 1e4*(elev'**2 + rudd'**2 + ail'**2 + flaps'**2)
  where
    elev = csElev surfs
    rudd = csRudder surfs
    ail = csAil surfs
    flaps = csFlaps surfs

    --elev' = csElev surfs'
    --rudd' = csRudder surfs'
    --ail' = csAil surfs'
    --flaps' = csFlaps surfs'


dae :: Floating a => Dae AcX None AcU None AcX AeroOutputs a
dae x' x _ u _ _ = (x' ^-^ xdot, aos)
  where
    (xdot, (_,aos)) = aircraftOde (mass, inertia) fcs mcs refs x u
    mass = bettyMass
    inertia = V3T (fmap V3T bettyInertia)
    fcs = bettyFc
    mcs = bettyMc
    refs = bettyRefs

ocp :: Vectorize bc
    => (forall a . Floating a => a -> AcX a -> AcX a -> J (Cov JNone) SX -> J (Cov JNone) SX -> a)
    -> (forall a . Floating a => AcX a -> AcX a -> bc a)
    -> OcpPhase AcX None AcU None AcX AeroOutputs bc PathC JNone JNone JNone
ocp m bc =
  OcpPhase { ocpMayer = m
           , ocpLagrange = lagrange
           , ocpDae = dae
           , ocpBc = bc
           , ocpPathC = pathc
           , ocpPathCBnds = pathcBnds
           , ocpBcBnds = fill (Just 0, Just 0)
           , ocpXbnd = xbnd
           , ocpUbnd = ubnd
           , ocpZbnd = None
           , ocpPbnd = None
--           , ocpTbnd = (Just 0.5, Just 0.5)
           , ocpTbnd = (Just 2, Nothing)

           , ocpSq = 0
           , ocpSbnd = jfill (Nothing,Nothing)
           , ocpSbc = \_ _ -> cat JNone
           , ocpSbcBnds = cat JNone
           , ocpSh = \_ _ -> cat JNone
           , ocpShBnds = cat JNone
           }

pathc :: x a -> z a -> u a -> p a -> AeroOutputs a -> a -> PathC a
pathc _ _ _ _ aos _ = PathC { pcAlphaDeg = aoAlphaDeg aos
                            , pcBetaDeg = aoBetaDeg aos
                            , pcAirspeed = aoAirspeed aos
                            }

pathcBnds :: PathC (Maybe Double, Maybe Double)
pathcBnds = PathC { pcAlphaDeg = (Just (-5), Just 10)
                  , pcBetaDeg = (Just (-10), Just 10)
                  , pcAirspeed = (Just 10, Just 200)
                  }


xbnd :: AcX (Maybe Double, Maybe Double)
xbnd = AcX { ac_r_n2b_n = V3T $ V3 (Nothing,Nothing) (Nothing,Nothing) (Nothing, Just (-1))
           , ac_v_bn_b = fill (Nothing,Nothing)
           , ac_R_n2b = Rot $ V3
                        (V3 unb unb unb)
                        (V3 unb unb unb)
                        (V3 unb unb pos)
           , ac_w_bn_b = fill (Just (-8*2*pi), Just (8*2*pi))
           }
  where
    unb = (Just (-1.2), Just 1.2)
    pos = (Just 0, Just 1.2)

d2r :: Floating a => a -> a
d2r d = d*pi/180

ubnd :: AcU (Maybe Double, Maybe Double)
ubnd =
  AcU
  ControlSurfaces { csElev = (Just (d2r (-10)), Just (d2r 10))
                  , csRudder = (Just (d2r (-10)), Just (d2r 10))
                  , csAil = (Just (d2r (-10)), Just (d2r 10))
                  , csFlaps = (Just (d2r (-0.01)), Just (d2r 0.01))
                  }
gliderVx0 :: Num a => a
gliderVx0 = 20

bc0 :: Floating a => AcX a -> AcX a -> Bc0 a
bc0 (AcX x0 v0 dcm0 w0) _ =
  Bc0 (AcX (x0 - V3T (V3 0 0 z0)) (v0 - V3T (V3 gliderVx0 0 0)) (dcm0 - Rot eye3) w0)


bc12 :: Floating a => AcX a -> AcX a -> Bc12 a
bc12 (AcX x0 v0 dcm0 w0) (AcX xF _ _ _) =
  Bc12 (AcX (x0 - V3T (V3 0 0 z0)) (v0 - V3T (V3 gliderVx0 0 0)) (dcm0 - Rot eye3) w0)
       (xF - V3T (V3 0 0 z0))

z0 :: Num a => a
z0 = -2

initialGuess :: CollTraj AcX None AcU None JNone NCollStages CollDeg (Vector Double)
initialGuess = makeGuess tf guessX (\_ ->  None) guessU None
  where
    guessX t = AcX { ac_r_n2b_n = V3T $ V3 (r*(sin (t*w))) (r*(1 - cos (t*w))) z0
                   , ac_v_bn_b = V3T $ V3 (w*r*(cos (t*w))) (w*r*(sin (t*w))) z0
                   , ac_R_n2b = Rot eye3 `compose` (toDcm (Rot quat))
                   , ac_w_bn_b = V3T $ V3 0 0 w
                   }
      where
        quat = Quaternion (cos(w*t/2)) (V3 0 0 (sin (w*t/2)))

    guessU = (\_ -> fill 0)

    w = 2*pi/tf
    tf = 10
    r = gliderVx0/w

{-
initialGuess :: CollTraj AcX None AcU None JNone NCollStages CollDeg (Vector Double)
initialGuess = makeGuess maxTime guessX (\_ ->  None) guessU None
  where
    guessX = (\t -> AcX { ac_r_n2b_n = V3 (maxTime * sin (pi*t/maxTime)) (maxTime * cos (2*pi*t/maxTime) - maxTime)  (-2)
                        , ac_v_bn_b = V3 30 0 0
                        , ac_R_n2b = eye3
                        , ac_w_bn_b = fill 0
                        , ac_u = fill 0
                        })
    guessU = (\_ -> fill 0)
-}

norm2 :: Num a => V3T f a -> a
norm2 (V3T (V3 x y z)) = x*x + y*y + z*z

mayer0 :: Floating a => a -> AcX a -> AcX a -> J (Cov JNone) SX -> J (Cov JNone) SX -> a
mayer0 t _ (AcX pF _ _ _) _ _ = t + 100*(norm2 (pF - V3T (V3 0 0 z0)))

mayer1 :: Floating a => a -> AcX a -> AcX a -> J (Cov JNone) SX -> J (Cov JNone) SX -> a
mayer1 t _ _ _ _ = t

mayer2 :: Floating a => a -> AcX a -> AcX a -> J (Cov JNone) SX -> J (Cov JNone) SX -> a
mayer2 t _ _ _ _ = -t

solver :: NlpSolverStuff
solver = ipoptSolver
--solver = snoptSolver

main :: IO ()
main = do
  nlp0 <- makeCollNlp $ ocp mayer0 bc0
  nlp1 <- makeCollNlp $ ocp mayer1 bc12
  nlp2 <- makeCollNlp $ ocp mayer2 bc12
  Zmq.withContext $ \context ->
    Zmq.withPublisher context chanDynoPlot $ \sendDynoPlotMsg ->
    Zmq.withPublisher context chanOptTelem $ \sendOptTelemMsg -> do
      let guess0 = cat initialGuess

          callback :: J (CollTraj AcX None AcU None JNone NCollStages CollDeg) (Vector Double)
                      -> IO Bool
          callback traj = do
            -- dynoplot
            let dynoPlotMsg = Zmq.encodeSerial (ctToDynamic traj, toMeta traj)
            sendDynoPlotMsg "dynoplot" dynoPlotMsg
            -- 3d vis
            let CollTraj tf' _ _ stages' xf = split traj
                stages :: [(CollStage (JV AcX) (JV None) (JV AcU) CollDeg) (Vector Double)]
                stages = map split $ F.toList $ unJVec (split stages')

                states :: [AcX Double]
                states = concatMap stageToXs stages ++ [jToX xf]

                stageToXs :: CollStage (JV AcX) (JV None) (JV AcU) CollDeg (Vector Double)
                             -> [AcX Double]
                stageToXs (CollStage x0 xzus) = [jToX x0]
--                stageToXs (CollStage x0 xzus) = jToX x0 : map getX points
--                  where
--                    points :: [CollPoint (JV AcX) (JV None) (JV AcU) (Vector Double)]
--                    points = map split (F.toList (unJVec (split xzus)))

                jToX = fmap V.head . unJV . split

                getX :: CollPoint (JV AcX) (JV None) (JV AcU) (Vector Double) -> AcX Double
                getX (CollPoint x _ _) = jToX x

                poses :: [Msg.AcPose]
                poses = map stateToPose states

                stateToPose :: AcX Double -> Msg.AcPose
                stateToPose acX = Msg.AcPose
                                    (toXyz (ac_r_n2b_n acX))
                                    (toDcmMsg (ac_R_n2b acX))
                tf :: Double
                tf = (V.head . unS . split) tf'
                msgs = [printf "final time: %.2f" tf]
                optTelemMsg = Msg.OptTelem (S.fromList poses) (S.fromList (map PB.fromString msgs))

            sendOptTelemMsg "opt_telem" (Zmq.encodeProto optTelemMsg)
            return True

      (msg0,opt0') <- solveNlp' solver (nlp0 { nlpX0' = guess0 }) (Just callback)
      opt0 <- case msg0 of Left msg' -> error msg'
                           Right _ -> return opt0'
      let guess1 = (xOpt' opt0) :: J (CollTraj AcX None AcU None JNone NCollStages CollDeg) (Vector Double)
      (msg1,opt1') <- solveNlp' solver (nlp1 { nlpX0' = guess1 }) (Just callback)
      opt1 <- case msg1 of Left msg' -> error msg'
                           Right _ -> return opt1'

      let guess2 = (xOpt' opt1) :: J (CollTraj AcX None AcU None JNone NCollStages CollDeg) (Vector Double)
      (msg2,opt2') <- solveNlp' solver (nlp2 { nlpX0' = guess2 }) (Just callback)
      opt2 <- case msg2 of Left msg' -> error msg'
                           Right _ -> return opt2'

--    let xopt = xOpt opt
--        lambda = lambdaOpt opt
--
--    snoptOpt' <- solveNlp snoptSolver (nlp {nlpX0 = xopt}) (Just callback) (Just lambda)
--    snoptOpt <- case snoptOpt' of Left msg -> error msg
--                                  Right opt'' -> return opt''
--    let xopt' = xOpt snoptOpt
--        lambda' = lambdaOpt opt
--        lambdax' = vectorize $ lambdaX lambda'
--        lambdag' = vectorize $ lambdaG lambda'
--    _ <- solveSqp (nlp {nlpX0 = xopt}) fullStep
--    _ <- solveSqp (nlp {nlpX0 = xopt}) armilloSearch

      return ()

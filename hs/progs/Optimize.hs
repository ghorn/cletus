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
import Dyno.Solvers
import Dyno.Nlp
import Dyno.NlpSolver ( solveNlp' )
import Dyno.Ocp
import Dyno.DirectCollocation
import Dyno.DirectCollocation.Quadratures
import Dyno.DirectCollocation.Types ( CollStage(..), CollPoint(..) )
import Dyno.DirectCollocation.Formulate ( makeGuess )
import Dyno.DirectCollocation.Dynamic
import Dyno.Nats

import Casadi.Option

import SpatialMathT
import MsgHelpers

import Model.Aircraft
import Model.AeroCoeffs
import Model.Betty

import qualified Protobetty.AcPose as Msg
import qualified Protobetty.OptTelem as Msg

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

lagrange :: Floating a => AcX a -> None a -> AcU a -> None a -> AeroOutputs a -> a -> a -> a
lagrange (AcX _pos _vit _ _) _ (AcU surfs) _ _ _ _endTime = 0
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


dae :: Floating a => AcX a -> AcX a -> t -> AcU a -> t1 -> t2 -> (AcX a, AeroOutputs a)
dae x' x _ u _ _ = (x' ^-^ xdot, aos)
  where
    (xdot, (_,aos)) = aircraftOde (mass, inertia) fcs mcs refs x u
    mass = bettyMass
    inertia = V3T (fmap V3T bettyInertia)
    fcs = bettyFc
    mcs = bettyMc
    refs = bettyRefs

ocp :: Vectorize bc
    => (forall a . Floating a => a -> AcX a -> AcX a -> a)
    -> (forall a . Floating a => AcX a -> AcX a -> bc a)
    -> OcpPhase AcX None AcU None AcX AeroOutputs bc PathC
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
           , ocpTScale = Nothing
           , ocpObjScale = Nothing
           , ocpXScale = Nothing
           , ocpZScale = Nothing
           , ocpUScale = Nothing
           , ocpPScale = Nothing
           , ocpResidualScale = Nothing
           , ocpBcScale = Nothing
           , ocpPathCScale = Nothing
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
    unb = (Just (-1.5), Just 1.5)
    pos = (Just 0, Just 1.5)

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

q0 :: Floating a => a
q0 = 0*pi/180

dcm0' :: Floating a => Rot N B (M33 a)
dcm0' = Rot eye3 `compose` toDcm (Rot (Quaternion (cos (q0/2)) (V3 0 (sin (q0/2)) 0)))

bc0 :: Floating a => AcX a -> AcX a -> Bc0 a
bc0 (AcX x0 v0 dcm0 w0) _ =
  Bc0 (AcX (x0 - V3T (V3 0 0 z0)) (v0 - V3T (V3 gliderVx0 0 0)) (dcm0 - dcm0') w0)


bc12 :: Floating a => AcX a -> AcX a -> Bc12 a
bc12 (AcX x0 v0 dcm0 w0) (AcX xF _ _ _) =
  Bc12 (AcX (x0 - V3T (V3 0 0 z0)) (v0 - V3T (V3 gliderVx0 0 0)) (dcm0 - dcm0') w0)
       (xF - V3T (V3 0 0 z0))

z0 :: Num a => a
z0 = -2

initialGuess :: CollTraj AcX None AcU None NCollStages CollDeg (Vector Double)
initialGuess = makeGuess Legendre tf guessX (\_ ->  None) guessU None
  where
    guessX t = AcX { ac_r_n2b_n = V3T $ V3 (r*(sin (t*w))) (r*(1 - cos (t*w))) z0
                   , ac_v_bn_b = V3T $ V3 (w*r) 0 0
                   , ac_R_n2b = Rot eye3 `compose` turn `compose` bank
                   , ac_w_bn_b = rot bank (V3T (V3 0 0 w))
                   }
      where
        turn = toDcm $ Rot $ Quaternion (cos(w*t/2)) (V3 0 0 (sin (w*t/2)))
        bank = toDcm $ Rot $ Quaternion (cos(qBank/2)) (V3 (sin (qBank/2)) 0 0)
        qBank = 30*pi/180

    guessU = (\_ -> fill 0)

    w = 2*pi/tf
    tf = 10
    r = gliderVx0/w

norm2 :: Num a => V3T f a -> a
norm2 (V3T (V3 x y z)) = x*x + y*y + z*z

mayer0 :: Floating a => a -> AcX a -> AcX a -> a
mayer0 t _ (AcX pF _ _ _) = t + 100*(norm2 (pF - V3T (V3 0 0 z0)))

mayer1 :: Floating a => a -> AcX a -> AcX a -> a
mayer1 t _ _ = t

mayer2 :: Floating a => a -> AcX a -> AcX a -> a
mayer2 t _ _ = -t

solver :: NlpSolverStuff
solver = ipoptSolver { options = [("linear_solver", Opt "ma86")] }
--solver = snoptSolver { options = [("detect_linear", Opt False)] }

main :: IO ()
main = do
--  (nlp0,toDyn0) <- makeCollNlp $ ocp mayer0 bc0
--  (nlp1,cb1) <- makeCollNlp $ ocp mayer1 bc12
  cp <- makeCollProblem $ ocp mayer2 bc12
  let nlp0 = cpNlp cp
      toDyn0 = cpCallback cp
  Zmq.withContext $ \context ->
    Zmq.withPublisher context chanDynoPlot $ \sendDynoPlotMsg ->
    Zmq.withPublisher context chanOptTelem $ \sendOptTelemMsg -> do
      let guess0 = cat initialGuess

          callback :: J (CollTraj AcX None AcU None NCollStages CollDeg) (Vector Double)
                      -> IO Bool
          callback traj = do
            (dyn,_) <- toDyn0 traj
            -- dynoplot
            let dynoPlotMsg = Zmq.encodeSerial (dyn, toMeta Legendre (Proxy :: Proxy AeroOutputs) (reproxy traj))
                reproxy :: J a b -> Proxy a
                reproxy = const Proxy
            sendDynoPlotMsg "dynoplot" dynoPlotMsg
            -- 3d vis
            let CollTraj tf' _ stages' xf = split traj
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
--      let guess1 = (xOpt' opt0) :: J (CollTraj AcX None AcU None JNone NCollStages CollDeg) (Vector Double)
--      (msg1,opt1') <- solveNlp' solver (nlp1 { nlpX0' = guess1 }) (Just callback)
--      opt1 <- case msg1 of Left msg' -> error msg'
--                           Right _ -> return opt1'
--
--      let guess2 = (xOpt' opt1) :: J (CollTraj AcX None AcU None JNone NCollStages CollDeg) (Vector Double)
--      (msg2,opt2') <- solveNlp' solver (nlp2 { nlpX0' = guess2 }) (Just callback)
--      opt2 <- case msg2 of Left msg' -> error msg'
--                           Right _ -> return opt2'
      return ()

{-# OPTIONS_GHC -Wall #-}
--{-# Language ScopedTypeVariables #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language RankNTypes #-}

module Main ( main ) where

import Linear
import Data.Vector ( Vector )
import GHC.Generics ( Generic )

import Dyno.Vectorize
import Dyno.View
import Dyno.Ipopt
--import Dyno.Snopt
--import Dyno.Sqp.Sqp
--import Dyno.Sqp.LineSearch
import Dyno.Nlp
import Dyno.NlpSolver

import Dyno.Ocp
import Dyno.DirectCollocation
import Dyno.Cov
import Dyno.DirectCollocation.Dynamic ( toMeta, ctToDynamic )
import Dyno.DirectCollocation.Formulate ( makeGuess )

import Dyno.Models.Aircraft
import Dyno.Models.AeroCoeffs
import Dyno.Models.Betty
import Dyno.Nats

import GliderShared
import ServerSender ( withCallback )

type NCollStages = D120
type CollDeg = D3

data Bc0 a  = Bc0  (AcX a)        deriving (Eq, Functor, Generic, Generic1, Show)
data Bc12 a = Bc12 (AcX a) (V3 a) deriving (Eq, Functor, Generic, Generic1, Show)
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

lagrange :: Floating a => AcX a -> None a -> AcU a -> None a -> None a -> a -> a
lagrange (AcX _pos _vit _ _ (AcU surfs)) _ (AcU surfs') _ _ _ = 0
-- + (scalProd pos pos)-- / sqrt (scalProd pos pos)
 + 0*(elev**2 + rudd**2 + ail**2 + flaps**2)
 + 1e4*(elev'**2 + rudd'**2 + ail'**2 + flaps'**2)
  where
    elev = csElev surfs
    rudd = csRudder surfs
    ail = csAil surfs
    flaps = csFlaps surfs

    elev' = csElev surfs'
    rudd' = csRudder surfs'
    ail' = csAil surfs'
    flaps' = csFlaps surfs'


dae :: Floating a => Dae AcX None AcU None AcX None a
dae x' x _ u _ _ = (aircraftDae (mass, inertia) fcs mcs refs x' x u, None)
  where
    mass = bettyMass
    inertia = bettyInertia
    fcs = bettyFc
    mcs = bettyMc
    refs = bettyRefs

ocp :: Vectorize bc
    => (forall a . Floating a => a -> AcX a -> AcX a -> J (Cov JNone) SX -> J (Cov JNone) SX -> a)
    -> (forall a . Floating a => AcX a -> AcX a -> bc a)
    -> OcpPhase AcX None AcU None AcX None bc None JNone JNone JNone
ocp m bc = OcpPhase { ocpMayer = m
               , ocpLagrange = lagrange
               , ocpDae = dae
               , ocpBc = bc
               , ocpPathC = pathc
               , ocpPathCBnds = None
               , ocpBcBnds = fill (Just 0, Just 0)
               , ocpXbnd = xbnd
               , ocpUbnd = ubnd
               , ocpZbnd = None
               , ocpPbnd = None
--               , ocpTbnd = (Just 0.5, Just 0.5)
               , ocpTbnd = (Just 2, Nothing)

               , ocpSq = 0
               , ocpSbnd = jfill (Nothing,Nothing)
               , ocpSbc = \_ _ -> cat JNone
               , ocpSbcBnds = cat JNone
               , ocpSh = \_ _ -> cat JNone
               , ocpShBnds = cat JNone
               }

pathc :: x a -> z a -> u a -> p a -> None a -> a -> None a
pathc _ _ _ _ _ _ = None

xbnd :: AcX (Maybe Double, Maybe Double)
xbnd = AcX { ac_r_n2b_n = V3 (Nothing,Nothing) (Nothing,Nothing) (Nothing, Just (-1))
           , ac_v_bn_b = fill (Nothing,Nothing)
           , ac_R_n2b = fill $ fill (Just (-1.2), Just 1.2)
           , ac_w_bn_b = fill (Just (-8*2*pi), Just (8*2*pi))
           , ac_u = ubnd
           }

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
bc0 (AcX x0 v0 dcm0 w0 cs) _ =
  Bc0 (AcX (x0 - V3 0 0 (-2)) (v0 - V3 gliderVx0 0 (0)) (dcm0 - eye3) w0 cs)


bc12 :: Floating a => AcX a -> AcX a -> Bc12 a
bc12 (AcX x0 v0 dcm0 w0 cs) (AcX xF _ _ _ _) =
  Bc12 (AcX (x0 - V3 0 0 (-2)) (v0 - V3 gliderVx0 0 (0)) (dcm0 - eye3) w0 cs)
       (xF - V3 0 0 (-2))

initialGuess :: CollTraj AcX None AcU None JNone NCollStages CollDeg (Vector Double)
initialGuess = makeGuess 10 guessX (\_ ->  None) guessU None
  where
    guessX = (\t -> AcX { ac_r_n2b_n = V3 (gliderVx0*t) 0 0
                        , ac_v_bn_b = V3 gliderVx0 0 0
                        , ac_R_n2b = eye3
                        , ac_w_bn_b = fill 0
                        , ac_u = fill 0
                        })
    guessU = (\_ -> fill 0)

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

mayer0 :: Floating a => a -> AcX a -> AcX a -> J (Cov JNone) SX -> J (Cov JNone) SX -> a
mayer0 t _ (AcX pF _ _ _ _) _ _ = t + 100*(norm (pF - V3 0 0 (-2)))**2

mayer1 :: Floating a => a -> AcX a -> AcX a -> J (Cov JNone) SX -> J (Cov JNone) SX -> a
mayer1 t _ _ _ _ = t

mayer2 :: Floating a => a -> AcX a -> AcX a -> J (Cov JNone) SX -> J (Cov JNone) SX -> a
mayer2 t _ _ _ _ = -t

main :: IO ()
main = do
  putStrLn $ "using ip \""++gliderUrl++"\""
  putStrLn $ "using channel \""++gliderChannelName++"\""

  nlp0 <- makeCollNlp $ ocp mayer0 bc0
  nlp1 <- makeCollNlp $ ocp mayer1 bc12
  nlp2 <- makeCollNlp $ ocp mayer2 bc12
  withCallback gliderUrl gliderChannelName $ \cb -> do
    let guess0 = cat initialGuess 

        cb' :: J (CollTraj AcX None AcU None JNone NCollStages CollDeg) (Vector Double) -> IO Bool
        cb' traj = cb (ctToDynamic traj, toMeta traj)

    (msg0,opt0') <- solveNlp' ipoptSolver (nlp0 { nlpX0' = guess0 }) (Just cb')
    opt0 <- case msg0 of Left msg' -> error msg'
                         Right _ -> return opt0'
    let guess1 = (xOpt' opt0) :: J (CollTraj AcX None AcU None JNone NCollStages CollDeg) (Vector Double)
    (msg1,opt1') <- solveNlp' ipoptSolver (nlp1 { nlpX0' = guess1 }) (Just cb')
    opt1 <- case msg1 of Left msg' -> error msg'
                         Right _ -> return opt1'

    let guess2 = (xOpt' opt1) :: J (CollTraj AcX None AcU None JNone NCollStages CollDeg) (Vector Double)
    (msg2,opt2') <- solveNlp' ipoptSolver (nlp2 { nlpX0' = guess2 }) (Just cb')
    opt2 <- case msg2 of Left msg' -> error msg'
                         Right _ -> return opt2'

--    let xopt = xOpt opt
--        lambda = lambdaOpt opt
--
--    snoptOpt' <- solveNlp snoptSolver (nlp {nlpX0 = xopt}) (Just cb) (Just lambda)
--    snoptOpt <- case snoptOpt' of Left msg -> error msg
--                                  Right opt'' -> return opt''
--    let xopt' = xOpt snoptOpt
--        lambda' = lambdaOpt opt
--        lambdax' = vectorize $ lambdaX lambda'
--        lambdag' = vectorize $ lambdaG lambda'
--    _ <- solveSqp (nlp {nlpX0 = xopt}) fullStep
--    _ <- solveSqp (nlp {nlpX0 = xopt}) armilloSearch

    return ()

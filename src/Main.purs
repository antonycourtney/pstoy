module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Lazy (Lazy,defer,force)
import Data.List.Lazy (List,range,zip,repeat,(:),nil) -- zip,repeat,(:),range)
import Data.Foldable (traverse_,foldl)
import Data.Tuple (Tuple(..))

type DTime = Number

type Transition a b = { outSample :: Lazy b, nextSF :: Lazy (SF a b) }

newtype SF a b = SF ( DTime -> Lazy a -> Transition a b)

liftSF :: forall a b . (a -> b) -> SF a b
liftSF f = SF \dt la -> { outSample: map f la, nextSF: pure (liftSF f) }

iPre :: forall a . a -> SF a a
iPre a0 = SF \dt la -> { outSample: pure a0, nextSF: map iPre la }

compSF :: forall a b c . SF a b -> SF b c -> SF a c
compSF (SF tf1) (SF tf2) = SF \dt la ->
  let
    t1 = tf1 dt la
    t2 = tf2 dt t1.outSample
  in { outSample: t2.outSample, nextSF : defer \ _ -> compSF (force t1.nextSF) (force t2.nextSF) }

tsf0 :: SF Int Int
tsf0 = liftSF (\x -> x + 10)

type AnimState a b = { nextSF :: SF a b, res :: List b }

-- Run an SF for one time step, forcing the result:
stepSF :: forall a b . AnimState a b -> Tuple DTime a -> AnimState a b
stepSF {nextSF: (SF tf), res: res} (Tuple dt a) =
  let
    t1 = tf dt (pure a)
    outSample = force t1.outSample
    nextSF = force t1.nextSF
  in
    { nextSF, res: outSample:res }

testSF1 :: SF Int Int -> List Int
testSF1 sf =
  let
    nSamples = 25
    sampleVals = range 0 nSamples
    dts = 0.0:repeat 0.25
    tas = zip dts sampleVals
    -- initial state:
    st0 = { res: nil, nextSF: sf }
    accSt = foldl stepSF st0 tas
  in
    accSt.res

main :: forall a. Eff (console::CONSOLE|a) Unit
main = do
  let vals = testSF1 tsf0
  log ("Hello, PureScript SF!")
  traverse_ (\x -> log (show x)) vals

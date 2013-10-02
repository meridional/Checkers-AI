module Training where

-- training module
-- used for... simulation of strategies
-- similar to minmax, but use different utilities
-- for min/max players
-- alpha-beta pruning does NOT work here

import Checkers
--import Control.Monad
import Control.Monad.Reader
--import Control.Monad.Trans
--import Control.Monad.Identity
import AlphaBeta
import Data.List (foldl1')
import Control.Monad.State
import Data.Monoid


type Training a = 
   ReaderT ((Cutoff,Eval),(Cutoff,Eval)) (State (Sum Int)) a

runTraining :: ((Cutoff,Eval),(Cutoff,Eval)) -> Training a -> (a, Sum Int)
runTraining c x = runState (runReaderT x c) (Sum 0) 

currentTrio :: Board -> Training (Cutoff, Eval, Int -> Int -> Bool)
currentTrio bo
  | playing bo == Red = asks fst >>= \(a,b) -> return (a,b,(>))
  | otherwise = asks snd >>= \(a,b) -> return (a,b,(<))

simulate :: Board -> Training Decision
simulate b = do
  (_,e,cmp) <- currentTrio b 
  let dfsf = if playing b == Red then minTrain else maxTrain
      r = expand b
  case r of
       EndGame x -> return $ Ended x
       Ongoing l -> do
         let pickFun list = fst $ pick list snd cmp
         fmap (Next . pickFun) $ mapM (\n -> dfsf 1 n >>= \d -> return (n, generalEval d e) ) l
         

callCutoffFun :: Int -> Board -> Report -> Training (Maybe Decision)
callCutoffFun _ _ (EndGame x) = return (Just $ Ended x)
callCutoffFun d b r@(Ongoing l) = do
  (c,e,cmp) <- currentTrio b
  if c d b r
     then return . Just . Next $ pick l e cmp
     else return Nothing
             

pick :: [a] -> (a->Int) -> (Int -> Int -> Bool) -> a
pick l e cmp = fst $ foldl1' (\(b,s) (b',s') -> if s `cmp` s' then (b,s)
                                                                   else (b',s')) (map (\bo -> (bo, e bo)) l)

generalEval :: Decision -> Eval -> Int
generalEval (Ended Black) _ = negate 999
generalEval (Ended _ ) _ = 999
generalEval (Next x) e = e x

maxTrain :: Int -> Board -> Training Decision
maxTrain d b = do
  increment (Sum 1)
  let r = expand b
      l = fromOngoing r
  de <- callCutoffFun d b r
  case de of
       Nothing -> do
         ds <- mapM (minTrain (d + 1)) l
         e <- asks (snd . fst)
         return $ pick ds (`generalEval` e) (>)
       Just x -> return x


minTrain :: Int -> Board -> Training Decision
minTrain d b = do
  increment (Sum 1)
  let r = expand b
      l = fromOngoing r
  de <- callCutoffFun d b r
  case de of
       Nothing -> do
         ds <- mapM (maxTrain (d + 1)) l
         e <- asks (snd . snd)
         return $ pick ds (`generalEval` e) (<)
       Just x -> return x

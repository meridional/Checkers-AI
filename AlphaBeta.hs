{-# LANGUAGE  OverloadedStrings #-}
module AlphaBeta where

import qualified Data.Text.Lazy as Text
import Checkers
import Control.Monad.State
import Control.Monad.Writer.Lazy
import Control.Monad.Reader

type Score = Int

type AIState = (Score,Score) 

alpha :: AIState -> Score
alpha = fst

beta :: AIState -> Score
beta = snd

type AI a =  ReaderT PruneConfig Maybe a

inf :: Score
inf = 99999

ninf :: Score
ninf = -99999

initialState :: AIState
initialState = (ninf*2, inf*2)


type Cutoff  =  Board -> Report -> Bool

type PruneConfig = (Bool, Cutoff, Eval) -- (pruning, cutoff check, eval fun)

pruneOrNot :: AI Bool
pruneOrNot = asks (\(b,_,_) -> b)

evalfun :: AI Eval
evalfun = asks (\(_,_,e) -> e)

cutofffun ::  AI Cutoff 
cutofffun = asks (\(_,f,_) -> f)

type Log a = (a, Text.Text)

getLog :: Log a -> Text.Text
getLog = snd

getValue :: Log a -> a
getValue = fst

runAI :: PruneConfig -> AI a -> Maybe a
runAI config x =  runReaderT x config

runabSearch :: Board -> PruneConfig -> Maybe Decision
runabSearch b c = runAI c (absearch b)


data Decision = Ended Color | Next Board deriving Show

--mmsearch :: Board -> AI Decision

absearch :: Board -> AI Decision
absearch b  
  | playing b == Red = case expand b of
                         EndGame x -> return (Ended x)
                         _ -> fmap (Next . fst) (maxSearch ninf inf b)
  | otherwise =  return (Ended Black)

--maxSearchReport :: Int -> Int -> Board -> AI (Board, Score)
--maxSearchReport aa bb b = do



type Eval = Board -> Score


callCutoff :: Board -> Report -> AI Score
callCutoff _ (EndGame Black) =  return ninf
callCutoff _ (EndGame Red) =  return inf
callCutoff b r = do
  cf <- cutofffun
  if cf b r then evalfun >>= \f -> return (f b)
            else mzero

maxSearch :: Int -> Int -> Board -> AI (Board, Score)
maxSearch aa bb b = do
  let report = expand b
      list = fromOngoing report
  (callCutoff b report >>= (\s -> return (b,s)))  `mplus` maxSearch' aa bb list b

maxSearch' :: Int -> Int -> [Board] -> Board -> AI (Board, Score)
maxSearch' aa _ [] b = return (b,aa) --gets alpha >>= \x -> return (b, x)
maxSearch' aa bb (h:rest) bestBoard = do
  s <- minSearchS aa bb h
  let newb = if s > aa then h else bestBoard
      aa' = max aa s
  pruning <- pruneOrNot
  if bb <= aa' && pruning then return (newb, aa')
            else maxSearch' aa' bb rest newb

fromOngoing :: Report -> [Board]
fromOngoing (Ongoing l) = l
fromOngoing _ = []

minSearchS :: Int -> Int -> Board -> AI Score 
minSearchS aa bb b = do
  let report = expand b
      list = fromOngoing report
  callCutoff b report `mplus` minSearchS' aa bb list 

minSearchS' :: Int -> Int -> [Board] -> AI Score
minSearchS' _ bb [] = return bb -- gets beta >>= \x -> return (b,x)
minSearchS' aa bb (h:rest) = do
  s <- maxSearchS aa bb h 
  let bb' = min bb s
  pruning <- pruneOrNot
  if bb' <= aa && pruning then return bb'
            else minSearchS' aa bb' rest 

maxSearchS :: Int -> Int -> Board -> AI Score
maxSearchS aa bb b = do
  let report = expand b
      list = fromOngoing report
  callCutoff b report `mplus` maxSearchS' aa bb list

maxSearchS' :: Int -> Int -> [Board] -> AI Score
maxSearchS' aa _ [] = return aa --gets alpha >>= \x -> return (b, x)
maxSearchS' aa bb (h:rest) = do
  s <- minSearchS aa bb h
  let aa' = max aa s
  pruning <- pruneOrNot
  if bb <= aa' && pruning then return aa'
            else maxSearchS' aa' bb rest 




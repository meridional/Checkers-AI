{-# LANGUAGE  OverloadedStrings #-}
module AlphaBeta where

--import qualified Data.Text.Lazy as Text
import Checkers
import Control.Monad.State.Strict
import Control.Monad.Reader
import Data.Monoid
import Control.Monad.Error
--import Control.Monad.Maybe

type Score = Int

type AIState = (Score,Score) 

alpha :: AIState -> Score
alpha = fst

beta :: AIState -> Score
beta = snd

type AI a = StateT (Sum Int) (ReaderT PruneConfig (ErrorT String IO)) a -- recording total number of nodes visited

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

type Log a = (a, Int)

getLog :: Log a -> Int
getLog = snd

getValue :: Log a -> a
getValue = fst

--runAI :: PruneConfig -> AI a -> Maybe (a, Sum Int)
runAI :: Num a1 =>r-> StateT (Sum a1) (ReaderT r (ErrorT e m)) a-> m (Either e (a, Sum a1))
runAI config x =  runErrorT $ runReaderT (runStateT x (Sum 0)) config

--runabSearch :: Board -> PruneConfig -> Maybe (Decision, Sum Int)
runabSearch :: Board -> PruneConfig -> IO (Either String (Decision, Sum Int))
runabSearch b c = runAI c (absearch b)


data Decision = Ended Color | Next Board 

instance Show Decision where
  show (Next b) = "Next:\n" ++ show b
  show (Ended c) = show c ++ " wins!\n"

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

increment :: (MonadState w m, Monoid w) => w -> m ()
increment a = do
  w <- get
  put $! w `mappend` a

maxSearch :: Int -> Int -> Board -> AI (Board, Score)
maxSearch aa bb b = do
  increment (Sum 1) 
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
  increment (Sum 1)
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
--  liftIO (putStrLn "expanding board" >> print b)
  increment (Sum 1)
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




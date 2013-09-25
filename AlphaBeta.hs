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


type AI a = StateT (Sum Int) (ReaderT PruneConfig (ErrorT String IO)) a -- recording total number of nodes visited

inf :: Score
inf = 999999

ninf :: Score
ninf = -999999

type Cutoff  =  Int -> Board -> Report -> Bool

type PruneConfig = (Bool, Cutoff, Eval) -- (pruning, cutoff check, eval fun)
type AIConfig = PruneConfig

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
absearch b = case expand b of
                  EndGame x -> return (Ended x)
                  _ -> do Next k <- fmap (Next . fst) (if playing b == Red 
                                                       then maxSearch 0 ninf inf b
                                                       else minSearch 0 ninf inf b)
                          if playing k == playing b then liftIO (putStrLn "failed") >> fail "WFT"
                                                    else return  $ Next k


type Eval = Board -> Score


callCutoff :: Int -> Board -> Report -> AI Score
callCutoff _ _ (EndGame Black) =  return (negate 999)
callCutoff _ _ (EndGame Red) =  return 999
callCutoff i b r = do
  cf <- cutofffun
  if cf i b r then evalfun >>= \f -> return (f b)
              else mzero

increment :: (MonadState w m, Monoid w) => w -> m ()
increment a = do
  w <- get
  put $! w `mappend` a

minSearch :: Int -> Int -> Int -> Board -> AI (Board, Score)
minSearch d aa bb b = do
  increment (Sum 1) 
  let report = expand b
      list = fromOngoing report
  (callCutoff d b report >>= (\s -> return (b,s)))  `mplus`  minSearch' d aa bb list b
   

minSearch' :: Int -> Int -> Int -> [Board] -> Board -> AI (Board, Score)
minSearch' _ _ be [] b = return (b,be) --gets alpha >>= \x -> return (b, x)
minSearch' d aa bb (h:rest) bestBoard = do
  s <- maxSearchS (d + 1) aa bb h 
  let newBoard = if s < bb then h else bestBoard
      bb' = min bb s
  pruning <- pruneOrNot
  if bb' <= aa && pruning then return (newBoard,bb')
            else minSearch' d aa bb' rest newBoard

maxSearch :: Int -> Int -> Int -> Board -> AI (Board, Score)
maxSearch d aa bb b = do
  increment (Sum 1) 
  let report = expand b
      list = fromOngoing report
  (callCutoff d b report >>= (\s -> return (b,s)))  `mplus` maxSearch' d aa bb list b

maxSearch' :: Int -> Int -> Int -> [Board] -> Board -> AI (Board, Score)
maxSearch' _ aa _ [] b = return (b,aa) --gets alpha >>= \x -> return (b, x)
maxSearch' d aa bb (h:rest) bestBoard = do
  s <- minSearchS (d + 1) aa bb h
  let newb = if s > aa then h else bestBoard
      aa' = max aa s
  pruning <- pruneOrNot
  if bb <= aa' && pruning then return (newb, aa')
            else maxSearch' d aa' bb rest newb

fromOngoing :: Report -> [Board]
fromOngoing (Ongoing l) = l
fromOngoing _ = []

minSearchS :: Int -> Int -> Int -> Board -> AI Score 
minSearchS d aa bb b = do
  increment (Sum 1)
  let report = expand b
      list = fromOngoing report
  callCutoff d b report `mplus` minSearchS' d aa bb list 

minSearchS' :: Int -> Int -> Int -> [Board] -> AI Score
minSearchS' _ _ bb [] = return bb -- gets beta >>= \x -> return (b,x)
minSearchS' d aa bb (h:rest) = do
  s <- maxSearchS (d + 1) aa bb h 
  let bb' = min bb s
  pruning <- pruneOrNot
  if bb' <= aa && pruning then return bb'
            else minSearchS' d aa bb' rest 

maxSearchS :: Int -> Int -> Int -> Board -> AI Score
maxSearchS d aa bb b = do
--  liftIO (putStrLn "expanding board" >> print b)
  increment (Sum 1)
  let report = expand b
      list = fromOngoing report
  callCutoff d b report `mplus` maxSearchS' d aa bb list

maxSearchS' :: Int -> Int -> Int -> [Board] -> AI Score
maxSearchS' _ aa _ [] = return aa --gets alpha >>= \x -> return (b, x)
maxSearchS' d aa bb (h:rest) = do
  s <- minSearchS (d + 1) aa bb h
  let aa' = max aa s
  pruning <- pruneOrNot
  if bb <= aa' && pruning then return aa'
            else maxSearchS' d aa' bb rest 




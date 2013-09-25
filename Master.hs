module Main where
import System.IO
import Checkers
import AlphaBeta
--import Control.Monad.Trans.Maybe
--import Data.Maybe
import qualified Data.Set as Set
import Data.Monoid
--import Data.List (foldl')
--import Control.Concurrent
--import qualified Data.Text.Lazy as Text

basicCutoff :: Int -> Cutoff
basicCutoff _ _ _ (EndGame _) = True
basicCutoff x d _ _
  | d <= x = False
  | otherwise = True

nojumpCutoff :: Int -> Cutoff
nojumpCutoff _ _ _ (EndGame _) = True
nojumpCutoff x d b (Ongoing (h:_)) 
  | d <= x = False
  | Set.size (opposingPieces b) > Set.size (playingPieces h) = False
  | otherwise = True

basicEval :: Eval
basicEval b = Set.size r - Set.size bl
  where r =  redps b
        bl = blackps b

kingEval :: Eval
kingEval b = Set.foldl' f 0 (redps b) + Set.foldl' g 0 (blackps b)
  where f a (King _ _) = a + kingVal
        f a _ = a + pawnVal
        g a (King _ _) = a - kingVal
        g a _ = a - pawnVal 
        kingVal = 5
        pawnVal = 1

offenceEval :: Eval
offenceEval b = Set.foldl' f 0 (redps b) + Set.foldl' f 0 (blackps b)
  where f a (King _ Red) = 10 + a
        f a (King _ Black) = a - 10
        f a (Pawn (_,y) _) = y + a

defenceEval :: Eval
defenceEval b = Set.foldl' f 0 (redps b) + Set.foldl' g 0 (blackps b)
  where f a pc = a + 2 - score [(x - 1, y - 1), (x + 1, y - 1)]
          where (x,y) = pos pc
        g a pc = a - 2 + score [(x - 1, y + 1), (x + 1, y + 1)]
          where (x,y) = pos pc
        score = length . filter (\t -> onTheBoard t && emptyTile' b t)
                              
defendKings :: Eval
defendKings b = Set.foldl' f 0 (redps b) - Set.foldl' f 0 (blackps b)
  where f a pc = a + 4 - score [(x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1), (x + 1, y + 1)]
          where (x,y) = pos pc
        score = length . filter (\t -> onTheBoard t && emptyTile' b t)

defenceOnTheSideEval :: Eval
defenceOnTheSideEval b = Set.foldl' f 0 (redps b) - Set.foldl' f 0 (blackps b)
  where f a pc = a + if x == 1 || x == 8 || y == 1 || y == 8 then 4
                                                             else 4 - score  [(x - 1, y - 1), (x + 1, y - 1), (x - 1, y + 1), (x + 1, y + 1)]
                    where (x,y) = pos pc
        score = length . filter (\t -> onTheBoard t && emptyTile' b t)

moveCountEval :: Eval
moveCountEval b = score b + score (flipBoard b)
  where score x = (if playing x == Red then id else negate) . length . fromOngoing . expand $ x
  
--minmax :: Int ->  Board -> Decision
minmax :: Int -> Board -> IO (Either String (Decision, Data.Monoid.Sum Int))
minmax x b =  runabSearch b (False, basicCutoff x, kingEval) 

--alphabeta :: Int -> Board -> Decision
alphabeta :: Int -> Board -> IO (Either String (Decision, Data.Monoid.Sum Int))
alphabeta x b =  runabSearch b (True, nojumpCutoff x, kingEval)

gameOn :: Board -> (AIConfig, AIConfig) -> IO ()
gameOn b (fstc, sndc) = do
   Right (d, _) <- runabSearch b fstc
   case d of 
        Ended _ -> print d
        Next b' -> do
            putStr "Turn "
            print (roundNumber b)
            putStr $ show (playing b)
            putStrLn " plays:\n" 
            print b'
            gameOn b' (sndc, fstc)



main ::  IO ()
main = do
--  h <- openFile "test5" ReadMode
  let h = stdin
  x <- fmap read $ hGetLine h :: IO Int
  putStrLn $ "Cutoff at " ++ show x ++ "\n"
  b <-  hGetLine h
  pcs <- fmap (map makePiece . lines) $ hGetContents h
  let bo = makeBoard b pcs
  print bo
--  print $ expand bo
  putStrLn "----------------"
  let evalA = (True, nojumpCutoff x, \bb -> 3 * kingEval bb + defenceOnTheSideEval bb + offenceEval bb)
      evalB = (True, nojumpCutoff 10, kingEval)
  gameOn bo (evalB, evalA)
  --d <- alphabeta x bo 
 -- print d
--  print $ expand bo
  --putStrLn $ Text.unpack . snd $ d
  --d

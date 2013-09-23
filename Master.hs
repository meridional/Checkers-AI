module Main where
import System.IO
import Checkers
import AlphaBeta
import Data.Maybe
import qualified Data.Set as Set
--import qualified Data.Text.Lazy as Text

basicCutoff :: Int -> Cutoff
basicCutoff _ _ (EndGame _) = True
basicCutoff x b _
  | roundNumber b < x = False
  | otherwise = True

basicEval :: Eval
basicEval b = Set.size r - Set.size bl
  where r = Set.filter (\x -> color x == Red) (pieces b)
        bl = Set.filter (\x -> color x == Black) (pieces b)

kingEval :: Eval
kingEval b = Set.foldl f 0 (pieces b)
  where f a (King _ Black) = a - 5
        f a (King _ _) = a + 5
        f a (Pawn _ Black) = a - 1
        f a _ = a + 1
  
minmax :: Int ->  Board -> Decision
minmax x b = fromJust $ runabSearch b (True, basicCutoff x, kingEval) 

main ::  IO ()
main = do
  h <- openFile "test5" ReadMode
  x <- fmap read $ hGetLine h
  putStrLn $ "Cutoff at " ++ show x ++ "\n"
  b <-  hGetLine h
  pcs <- fmap (map makePiece . lines) $ hGetContents h
  let bo = makeBoard b pcs
  print bo
  let d = minmax x bo 
--  print $ expand bo
  putStrLn "----------------"
  --putStrLn $ Text.unpack . snd $ d
  print $ d

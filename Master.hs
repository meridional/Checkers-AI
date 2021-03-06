module Main where
import System.IO
import Checkers
import AlphaBeta
import Training
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

dynamicNoJumpCutoff :: Int -> Cutoff
dynamicNoJumpCutoff _ _ _ (EndGame _) = True
dynamicNoJumpCutoff x d b (Ongoing (h:_)) 
  | d <=  (12 * x `div` (Set.size (playingPieces b)+6)) = False
  | Set.size (opposingPieces b) > Set.size (playingPieces h) = False
  | otherwise = True


basicEval :: Eval
basicEval b = Set.size r - Set.size bl
  where r =  redps b
        bl = blackps b

kingEval ::Int -> Eval
kingEval kingVal b = Set.foldl' f 0 (redps b) + Set.foldl' g 0 (blackps b)
  where f a (King _ _) = a + kingVal
        f a _ = a + pawnVal
        g a (King _ _) = a - kingVal
        g a _ = a - pawnVal 
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
minmax x b =  runabSearch b (False, basicCutoff x, basicEval) 

--alphabeta :: Int -> Board -> Decision
alphabeta :: Int -> Board -> IO (Either String (Decision, Data.Monoid.Sum Int))
alphabeta x b =  runabSearch b (True, nojumpCutoff x, kingEval 5)

gameOn :: Board -> (AIConfig, AIConfig) -> IO ()
gameOn b (fstc, sndc) 
 | roundNumber b > steps = putStrLn $ show steps ++ " has been reached.\n"
 | otherwise = do
   putStrLn "Alpha-beta pruning"
   Right (d, Sum n) <- runabSearch b fstc
   putStrLn "----------------------"
   putStrLn $ show n ++ " nodes visited"
   case d of 
        Ended _ -> print d
        Next b' -> do
            putStr "Turn "
            print (roundNumber b)
            putStr $ show (playing b)
            putStrLn " plays:\n" 
            print b'
            gameOn b' (sndc, fstc)

steps :: Int
steps = 80 

type Reflection = Board -> Decision
gameOn' :: Board -> (Reflection, Reflection) -> IO ()
gameOn' b (f,s)
 | roundNumber b == steps = putStrLn $ show steps ++ " has been reached.\n"
 | otherwise = do
    putStrLn "MINMAX strategy"
    let b' = f b
    putStrLn $ "Turn " ++ show (roundNumber b) ++ ":"
    case b' of
      Ended _ -> print b'
      Next x -> putStrLn (show (playing b) ++ " plays:" ) >>
                     print x >> gameOn' x (s,f)

makeTrainingDuo :: Cutoff -> Eval -> Eval -> (Reflection,Reflection)
makeTrainingDuo c e1 e2 = (fst . runTraining ((c, e1), (c, e2)) . simulate, fst . runTraining ((c,e2),(c,e1)) . simulate )

main ::  IO ()
main = do
--  h <- openFile "test5" ReadMode
  let h = stdin
  x <- fmap read $ hGetLine h :: IO Int
  putStrLn $ "Cutoff at " ++ show x ++ "\n"
  b <-  hGetLine h
  pcs <- fmap (map makePiece . lines) $ hGetContents h
  let bo = makeBoard b pcs
  putStrLn "Initial Board:"
  print bo
--  print $ expand bo
  let evalA = (True, basicCutoff 8, kingEval 5)
      evalB = (True, dynamicNoJumpCutoff 1, kingEval 1)
  gameOn bo (evalA, evalB)
  let duo = makeTrainingDuo (dynamicNoJumpCutoff 6) (kingEval 2) (kingEval 5)
  --gameOn' bo duo
  return ()

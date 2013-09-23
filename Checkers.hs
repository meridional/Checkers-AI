module Checkers where

import Control.Concurrent
import Control.Monad.Reader
import Data.Maybe
import Data.List (find, intersperse , sortBy)
import qualified Data.Set as Set
import System.IO
import Data.Ord 

type Tile = (Int, Int)

compTile :: (Int , Int ) -> (Int, Int) -> Ordering
compTile (x,y) (a,b)
  | x == a = y `compare` b
  | otherwise = x `compare` a

data Color = Black | Red deriving (Show, Eq, Ord)

data Piece = Pawn {
    pos :: Tile
  , color :: Color
  }
  | King {
    pos :: Tile
  , color :: Color
  }
  deriving (Show, Eq)

instance Ord Piece where
  x `compare` y = pos x `compTile` pos y


data Board = Board {
    pieces :: Set.Set Piece
  , playing :: Color
  , roundNumber :: Int
  } 

instance Show Board where
  show b = let fstLine = " 12345678"
             in unlines . map (intersperse ' ') $ fstLine : map format [1..8]
     where
      pcs = Set.toList $ pieces b
      format n = show n ++ map (format' n) [1..8]
      format' y x = case find (\p -> pos p == (x,y)) pcs of
                         Just k -> piece2Char k
                         Nothing -> '+'

piece2Char :: Piece -> Char
piece2Char (Pawn _ Red) = 'r'
piece2Char (Pawn _ Black) = 'b'
piece2Char (King _ Black) = 'B'
piece2Char (King _ Red) = 'R'

flipBoard :: Board -> Board
flipBoard (Board p c r) = Board p (adv c) (r+1)

adv :: Color -> Color
adv Red = Black
adv Black = Red

playingPieces :: Board -> Set.Set Piece
playingPieces b = Set.filter (\p -> color p == playing b) (pieces b)

type Move = [Tile]

data Report = EndGame Color | Ongoing [Board] deriving (Show)

-- computations

data GameState = GameState {
    board :: Board
  }


type GameMechanics a = Reader GameState a

neighboringSteps :: Piece -> [Tile]
neighboringSteps (Pawn t Black) = backwardSteps t
neighboringSteps (Pawn t Red) = forwardSteps t
neighboringSteps (King t _) = backwardSteps t ++ forwardSteps t


type Step = Tile
type Jump = (Tile, Tile)

onTheBoard :: Tile -> Bool
onTheBoard (x,y) = x >= 1 && x <= 8 && y >= 1 && y <= 8

forwardSteps :: Tile -> [Tile]
forwardSteps (x,y) = filter onTheBoard [(x+1,y+1), (x-1,y+1)]

backwardSteps :: Tile -> [Tile]
backwardSteps (x,y) = filter onTheBoard [(x+1,y-1),(x-1,y-1)]

deletePiece :: Piece -> Board -> Board
deletePiece p (Board pcs ping r) = Board (Set.delete p pcs) ping r

occupied :: Tile -> Board -> Maybe Color
occupied t b = do
  p <- Set.lookupLE (genericPieceOnTile t) pcs
  if pos p == t then return (color p)
                else Nothing
  where pcs = pieces b
  

emptyTile :: Board -> Tile -> Bool
emptyTile b t = isNothing $ occupied t b 

paces :: Piece -> Board -> [Board]
paces p b = do
  t <- filter (emptyTile b) $ neighboringSteps p
  return $ commitPace p t b

changePos :: Piece -> Tile -> Piece
changePos (Pawn _ Black) t@(_, 1)  = King t Black
changePos (Pawn _ Red) t@(_,8)  = King t Red
changePos (Pawn _ c) t = Pawn t c
changePos (King _ c) t = King t c

commitPace :: Piece -> Tile -> Board -> Board
commitPace p t = putBack $ changePos p t

onTheBoard' :: Jump -> Bool
onTheBoard' (_,t') =  onTheBoard t'

possibleJumps :: Piece -> [Jump]
possibleJumps (Pawn (x,y) Black) = filter onTheBoard' [((x+1,y-1), (x+2,y-2)), ((x-1,y-1),(x-2,y-2))]
possibleJumps (Pawn (x,y) Red) = filter onTheBoard' [((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2))]
possibleJumps (King (x,y) _) = filter onTheBoard' [((x+1,y-1), (x+2,y-2)), ((x-1,y-1),(x-2,y-2)), ((x+1,y+1),(x+2,y+2)),((x-1,y+1),(x-2,y+2))]

kinged :: Piece -> Tile -> Bool
kinged (Pawn _ Black) (_, 1) = True
kinged (Pawn _ Red) (_, 8) = True
kinged _ _ = False

jumps :: Piece -> Board -> [Board]
jumps p b@(Board pcs c r) = do
  (t1, t2) <- filter (canJump b) (possibleJumps p)
  let newB = Board (Set.delete (genericPieceOnTile t1) pcs) c r
      newP = changePos p t2
  if kinged p t2 then return (putBack newP newB)
                 else jumps' newP newB

putBack :: Piece -> Board -> Board
putBack p (Board pcs c r) = Board (Set.insert p pcs) c r

deleteBy :: (a -> Bool) -> [a] -> [a]
deleteBy _ [] = []
deleteBy f (a:rest) = if f a then rest else a : deleteBy f rest

genericPieceOnTile :: Tile -> Piece
genericPieceOnTile t = Pawn t Red

jumps' :: Piece -> Board -> [Board]
jumps' p b@(Board pcs c r) = let js = filter (canJump b) (possibleJumps p)
                             in if null js then return (putBack p b)
                                           else do
                                             (t1,t2) <- js;
                                             let newB = Board (Set.delete (genericPieceOnTile t1) pcs) c r
                                                 newP = changePos p t2
                                             if kinged p t2 then return (putBack newP newB)
                                                            else jumps' newP newB

canJump :: Board -> Jump -> Bool
canJump b@(Board _ c _) (t1, t2) = occupied t1 b == Just (adv c) && emptyTile b t2

next :: GameMechanics Report
next = do
  pcs <- asks (playingPieces . board)
  b <- asks board
  let deletedPair = map (\p -> (p, deletePiece p b)) (Set.toList pcs)
      js = concatMap (\(p,ns) -> jumps p ns) deletedPair
      ps = concatMap (\(p,ns) -> paces p ns) deletedPair
  merge js ps
  where
    liftAndSort = return . Ongoing . map flipBoard -- . sortBy (comparing ( Set.size . pieces))
    merge js ps
      | not (null js) = liftAndSort js
      | not (null ps) = liftAndSort ps
      | otherwise = liftM (EndGame . adv . playing . board) ask


makeBoard :: String -> [Piece] -> Board
makeBoard p pcsL = Board (Set.fromList pcsL) pp 1
  where pp = if head p == 'R' then Red else Black

makePiece :: String -> Piece
makePiece s = let cs:k:x:y:_ = words s
                  t = (read x, read y)
                  f = if head k == 'K' then King else Pawn
                  c = if head cs == 'R' then Red else Black
                  in f t c

expand :: Board -> Report
expand b =  runReader next (GameState b)

readBoardFromFile :: FilePath -> IO Board
readBoardFromFile f = do
  h <- openFile f ReadMode
  b <- hGetLine h
  pcs <- fmap (map makePiece . lines) (hGetContents h)
  return $ makeBoard b pcs


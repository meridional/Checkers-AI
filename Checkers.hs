module Checkers where

--import Control.Concurrent
import Control.Monad.Reader
--import Data.Maybe
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
    redps :: Set.Set Piece
  , blackps :: Set.Set Piece
  , playing :: Color
  , roundNumber :: Int
  } 

instance Show Board where
  show b = let fstLine = " 12345678"
             in unlines . map (intersperse ' ') $ fstLine : map format [1..8]
     where
      pcs = Set.toList (redps b) ++ Set.toList (blackps b)
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
flipBoard (Board rp bp c r) = Board rp bp (adv c) (r+1)

adv :: Color -> Color
adv Red = Black
adv Black = Red

playingPieces :: Board -> Set.Set Piece
playingPieces b
  | playing b == Red = redps b
  | otherwise = blackps b

opposingPieces :: Board -> Set.Set Piece
opposingPieces b
  | playing b == Black = redps b
  | otherwise = blackps b
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
deletePiece p (Board rp bp c r) 
  | color p == Red = Board (Set.delete p rp) bp c r
  | otherwise = Board rp (Set.delete p bp) c r


emptyTile :: Board -> Piece -> Bool
emptyTile b p
  | color p == Red = p `Set.notMember` redps b
  | otherwise = p `Set.notMember` blackps b

emptyTile' :: Board -> Tile -> Bool
emptyTile' b t = emptyTile b (genericPieceOnTile t Red) && emptyTile b (genericPieceOnTile t Black)

paces :: Piece -> Board -> [Board]
paces p b = do
  t <- filter (emptyTile' b) $ neighboringSteps p
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
jumps p b = do
  (t1, t2) <- filter (canJump b) (possibleJumps p)
  let newB = deletePiece (genericPieceOnTile t1 (adv $ color p)) b 
      newP = changePos p t2
  if kinged p t2 then return (putBack newP newB)
                 else jumps' newP newB

putBack :: Piece -> Board -> Board
putBack p (Board rp bp c r)
  | color p == Red = Board (Set.insert p rp) bp c r
  | otherwise = Board rp (Set.insert p bp) c r

deleteBy :: (a -> Bool) -> [a] -> [a]
deleteBy _ [] = []
deleteBy f (a:rest) = if f a then rest else a : deleteBy f rest

genericPieceOnTile :: Tile -> Color -> Piece
genericPieceOnTile = Pawn 

jumps' :: Piece -> Board -> [Board]
jumps' p b = let js = filter (canJump b) (possibleJumps p)
                             in if null js then return (putBack p b)
                                           else do
                                             (t1,t2) <- js;
                                             let newB = deletePiece (genericPieceOnTile t1 (adv $ color p)) b
                                                 newP = changePos p t2
                                             if kinged p t2 then return (putBack newP newB)
                                                            else jumps' newP newB

canJump :: Board -> Jump -> Bool
canJump b (t1, t2)
  | playing b == Red = not (emptyTile b (genericPieceOnTile t1 Black)) && emptyTile' b t2
  | otherwise = not (emptyTile b (genericPieceOnTile t1 Red)) && emptyTile' b t2

next :: GameMechanics Report
next = do
  pcs <- asks (playingPieces . board)
  b <- asks board
  let deletedPair = map (\p -> (p, deletePiece p b)) (Set.toList pcs)
      js = concatMap (uncurry jumps) deletedPair
      ps = concatMap (uncurry paces) deletedPair
  merge js ps
  where
    liftAndSort = return . Ongoing . map flipBoard -- . sortBy (comparing ( Set.size . pieces))
    merge js ps
      | not (null js) = liftAndSort js
      | not (null ps) = liftAndSort ps
      | otherwise = liftM (EndGame . adv . playing . board) ask


makeBoard :: String -> [Piece] -> Board
makeBoard p pcsL = Board (Set.fromList redPiece) (Set.fromList blackPiece) pp 1
  where pp = if head p == 'R' then Red else Black
        redPiece = filter (\x -> color x == Red) pcsL
        blackPiece =filter (\x -> color x == Black) pcsL 

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


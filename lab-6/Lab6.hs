-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}

module Lab6 where

import Data.List
import Data.Tree

import System.Random

import Types
import DomViz
{- Uncomment the previous line if you want to use the visualization routines. -}

board4x4_3 = Board { turn = H,
                     free = [(1,1),(1,2),(2,2),(2,3),(2,4),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)],
                     hist = [(1,3),(2,1)] }

-- given a cell c and a player p, compute the adjacent cell c'
-- that is also occupied if p plays a domino at c
adjCell :: Cell -> Player -> Cell
adjCell (x,y) H = (x+1,y)
adjCell (x,y) V = (x,y+1)

-- compute the opponent of a player
opp :: Player -> Player
opp H = V
opp V = H

-- determine whether a move is valid in a given board
valid :: Board -> Cell -> Bool
valid b c = c `elem` free b && adjCell c (turn b) `elem` free b

-- create an empty board from an arbitrary list of cells
empty :: [Cell] -> Board
empty cs = Board { turn = H, free = cs, hist = [] }

-- create a rectangular board of arbitrary dimensions
board :: Int -> Int -> Board
board maxx maxy = empty [(x,y) | x <- [1..maxx], y <- [1..maxy]]

-- create a crosshatch-shaped square board of arbitrary dimension
hatch :: Int -> Board
hatch n = empty [(x,y) | x <- [1..2*n+1], y <- [1..2*n+1], odd y || x == 1 || x == (2*n+1) || odd x]

alphaDom_vs_LeeSedom =
  Board { turn = V,
          free = [(-4,1),(-4,3),(-2,0),(-2,4),(2,1),(2,4),(3,-4),(3,4),(4,-2),(4,0)],
          hist = [(0,4),(4,1),(0,-4),(-4,-3),(-1,-2),(2,-1),(-2,-4),(-4,-1),(-1,2),(4,3),(1,2),(-2,2),(-4,-4),(-2,-2),(2,-2),(4,-4),(-3,1),(2,-4),(-4,4),(-1,3),(-4,2),(-3,-2),(3,-1),(1,-3),(-2,-3),(3,1),(1,3)] }

alphaDom_vs_RanDom =
  Board { turn = V,
          free = [(-4,-3),(-4,0),(-2,-4),(-2,-2),(-1,-4),(-1,-2),(-1,2),(-1,4),(0,-4),(0,-2),(0,2),(0,4),(1,-4),(1,-2),(1,2),(1,4),(2,-4),(2,-2),(2,4),(3,-4),(4,0),(4,3)],
          hist = [(-3,4),(2,-1),(-3,2),(4,-2),(-4,-4),(-4,3),(3,4),(2,1),(-3,1),(3,1),(-4,-1),(-2,-1),(-2,3),(-4,1),(1,3),(4,-4),(-4,-2),(4,1),(1,-3),(3,-2),(-2,-3)] }

-- Exercise 1a
legalMoves :: Player -> Board -> [Cell]
legalMoves p b = [mv | mv <- (free b), (adjCell mv p) `elem` (free b)]

-- Exercise 1b
moveLegal :: Board -> Cell -> Board
moveLegal b mv = Board {
  turn = opp (turn b),
  free = foldr (\cell lis -> if (cell==mv || cell==(adjCell mv (turn b))) then lis else (cell:lis)) [] (free b),
  hist = (mv:(hist b))
}

-- Exercise 1c Auxiliar Functions

-- isEmpty b = returns if board is empty.
isEmpty :: Board -> Bool
isEmpty b = null (hist b)

-- rollback b = returns the board one move back in time. 
-- Assumes that hist b is non empty.
rollback :: Board -> Board
rollback b = let c1 = head (hist b) in 
  let c2 = adjCell c1 (opp (turn b)) in 
    Board{
      turn = opp (turn b),
      free = (c1:c2:(free b)),
      hist = tail (hist b)
    } 

-- initialBoard b = returns the initial board of this game
initialBoard :: Board -> Board
initialBoard b = if (isEmpty b) then b else initialBoard (rollback b)

-- Exercise 1c
playUntilPosition :: Board -> [Cell] -> [Board]
playUntilPosition b moveSequence = if (null moveSequence)
  then [b]
  else (b:(playUntilPosition bnext (tail moveSequence))) where
    bnext = moveLegal b (head moveSequence)

replay :: Board -> [Board]
replay b = playUntilPosition (initialBoard b) (reverse (hist b))

gametree :: Board -> Tree Board
gametree b = Node b [gametree (moveLegal b c) | c <- legalMoves (turn b) b]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

-- Exercise 2a
sign :: Player -> Int
sign H = -1
sign V =  1

score :: Board -> Score
score b = if (null (legalMoves (turn b) b)) 
  then Win (opp (turn b))
  else Heu ( (length (legalMoves V b)) - (length (legalMoves H b)) - sign (turn b) )

-- Exercise 2b
minimax :: (Board -> Score) -> Tree Board -> Tree (Board, Score)
minimax sfn (Node b lis) = let childrenMiniMax = map (\t -> minimax sfn t) lis in 
  if null childrenMiniMax
    then Node (b, sfn b) []
    else Node (b, value) childrenMiniMax
      where value = if (turn b) == H
              then min [childValue | (Node (child, childValue) _ ) <- childrenMiniMax]
              else max [childValue | (Node (child, childValue) _ ) <- childrenMiniMax]


-- Exercise 2c
bestmoves :: Int -> (Board -> Score) -> Board -> [Cell]
bestmoves = undefined

selectSafe :: SelectMonad m => [a] -> m (Maybe a)
selectSafe [] = return Nothing
selectSafe xs = select xs >>= \x -> return (Just x)
   
randomBestPlay :: SelectMonad m => Int -> (Board -> Score) -> Board -> m (Maybe Cell)
randomBestPlay d sfn = selectSafe . bestmoves d sfn

randomPlay :: SelectMonad m => Board -> m (Maybe Cell)
randomPlay b = selectSafe (legalMoves (turn b) b)

-- Exercise 3a
runGame :: SelectMonad m => (Board -> m (Maybe Cell)) -> (Board -> m (Maybe Cell)) -> Board -> m Board
runGame = undefined

-- Exercise 3b (optional)
carpets :: [Board]
carpets = undefined

-- alpha-beta pruning (optional)


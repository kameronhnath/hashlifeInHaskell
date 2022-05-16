{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Hashlife where

{- Kameron Hnath
   5/16/22
   CMSC488B Final Project -}

import Control.Applicative ((<|>))
import Control.Monad (guard)
import System.Random (Random(..), newStdGen)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.Memo
import Test.QuickCheck



{- Datatypes: 
Loc: x,y coordinate pair used to build Grid instances.
QCell: 'Quadtree' structure used to store information for the Hashlife algorithm. Each instance of QCell contains an int representing the 'height' at that node, and four other QCells.
A leaf node represents an individual cell in the game, with a 1 for a live cell and a 0 for a dead cell.
Grid: Uses a map to run the iterative version of the game of life. Stores the dimension of the grid. -}

type Loc = (Int, Int)

data QCell = Q Int QCell QCell QCell QCell 
  | C Int deriving (Eq, Ord)

data Grid = G Int (Map Loc Int) deriving (Eq, Ord)



{- 
Show instances allow the user to view grids and QCells in a grid like format. For example (Q 1 lc lc dc lc) would appear as:
1 1
0 1
-}
instance Show Grid where
  show (G d g) = showHelp 0 (G d g) 

instance Show QCell where
  show (C x) = show x
  show q = show (qcellToGrid q)

--Helper function that moves through the grid
showHelp :: Int -> Grid -> String
showHelp n (G d g) | n == (d-1) = addToS [(x,n) | x <- [0..d-1]] g
showHelp n (G d g) = (addToS [(x,n) | x <- [0..d-1]] g) ++ "\n" ++ showHelp (n+1) (G d g)

--Helper function for showHelp
addToS :: [Loc] -> Map Loc Int -> String
addToS (c:cs) g = case (g Map.!? c) of 
  Just x -> (show x) ++ " " ++ addToS cs g
  Nothing -> addToS cs g
addToS [] _ = ""



{- 
Iterative version of Conway's game of life. Sums the set of living neighbors for a given location in the grid and then apply the rules of the game.
-}

--Edge cases for nodes on the border of the grid (space outside is treated as a 0)
neighbors :: Int -> Loc -> [(Loc)]
neighbors d (0,0)                             = [(1,0),(0,1),(1,1)]
neighbors d (0,y) | y==d-1                    = [(1,y),(0,y-1),(1,y-1)]
neighbors d (x,0) | x==d-1                    = [(x-1,0),(x,1),(x-1,1)]
neighbors d (x,y) | x==d-1 && y==d-1          = [(x-1,y),(x,y-1),(x-1,y-1)]
neighbors d (0,y)                             = [(1,y),(0,y-1),(0,y+1),(1,y-1),(1,y+1)]
neighbors d (x,y) | x==d-1                    = [(x-1,y),(x,y-1),(x,y+1),(x-1,y-1),(x-1,y+1)]
neighbors d (x,0)                             = [(x,1),(x-1,0),(x+1,0),(x-1,1),(x+1,1)]
neighbors d (x,y) | y==d-1                    = [(x,y-1),(x-1,y),(x+1,y),(x+1,y-1),(x-1,y-1)]
neighbors d (x,y)                             = [(x+1,y),(x-1,y),(x,y-1),(x,y+1),(x-1,y+1),(x+1,y+1),(x-1,y-1),(x+1,y-1)]

--Counts the number of live neighbors given a set of neighbors
numNeighbors :: Grid -> Loc -> Int
numNeighbors (G d g) loc = foldr (checkLoc g) 0 (neighbors d loc) where
  checkLoc g loc count = case (Map.lookup loc g) of 
    Just x -> x + count
    _ -> count

--Updates a given cell to the next generation
cellNextGen :: Grid -> Loc -> (Loc,Int)
cellNextGen gr@(G d g) loc = case (g Map.!? loc) of
  Just 1 -> if (numNeighbors gr loc) == 3 || (numNeighbors gr loc) == 2 then (loc,1) else (loc,0)
  Just 0 -> if (numNeighbors gr loc) == 3 then (loc,1) else (loc,0)
  _ -> (loc,0)

--Moves a grid to the next generation using a map across the Map's keys
nextGen :: Grid -> Grid
nextGen gr@(G d g) = G d (Map.fromList $ map (cellNextGen gr) (Map.keys g))



{- I need to test to ensure that my implementation of the hashlife algorithm preserves the rules of Conway's Game of Life. The easiest way to do this is to check that the iterative
  implementation works the same way. Below I define an arbitrary instance for QCells, a way to convert between grids and QCells, a property to ensure this conversion is correct, 
  and some helper methods to use in the desired property. 
  
  It is important to note that the hashlife algorithm does work exactly the same as my implementation of the iterative algorithm. The hashlife algorithm only computes the next generation for the 
  middle portion of the grid (for example: running the algorithm on a 4x4 grid will output a 2x2 grid). The workaround for this issue is to 'expand' the grid whenever the algorithm runs by surrounding the
  relevant bit with 0s. However, in doing this, the algorithm will 'remember' living cells in the expanded whitespace due to memoization. The iterative algorithm does not have infinite space, and thus will
  not 'remember' living cells outside the border of the grid. But that is fine, we can test the functionality of the algorithm anyways. We will run the iterative and hashlife algorithm on the same initial
  grid, and then shrink the result of the iterative grid to match the same space as the hashlife grid. Thus, this will only work for QCells with height > 2. QCells with height > 6 run too slowly
  with the iterative algorithm.

  Also, the hashlife algorithm does not output a grid one generation in the future. Because it is a recursive algorithm that calls nextgen on many cells to reach the result, the algorithm actually moves
  2^(h-2) generations ahead in time, where h is the height of the initial QCell. I put a helper method below to run the iterative version of the game any number of times.
-}

--Generates arbitrary QCells of height 1-5 with each location having a 50% chance of being alive.
instance Arbitrary QCell where
  arbitrary = do
    size <- chooseInt (1,6)
    cell <- genQCell size
    return cell
  shrink (C _) = []
  shrink (Q n nw ne sw se) = [nw,ne,sw,se] ++ (shrink nw) ++ (shrink nw) ++ (shrink sw) ++ (shrink se)

genQCell :: Int -> Gen QCell
genQCell 0 = do
  x <- oneof [pure (C 0),pure (C 1)]
  return x
genQCell n = do
  nw <- genQCell (n-1)
  ne <- genQCell (n-1)
  sw <- genQCell (n-1)
  se <- genQCell (n-1)
  return $ Q n nw ne sw se

--Converts a QCell to a grid using the assemble map helper function to assemble the map part of the grid
qcellToGrid :: QCell -> Grid
qcellToGrid q@(C _) = G 0 (Map.fromList (assembleMap (0,0) q))
qcellToGrid q@(Q n _ _ _ _) = G (2^n) (Map.fromList (assembleMap (0,0) q))

--Keeps track of the top left corner of each recursive call to assembleMap in order to create the grid
assembleMap :: Loc -> QCell -> [(Loc,Int)]
assembleMap (x,y) (C b) = [((x,y),b)]
assembleMap (x,y) (Q n nw ne sw se) = let shift = (2^n) `div` 2 in
  (assembleMap (x,y) nw) ++ (assembleMap (x+shift,y) ne) ++ (assembleMap (x,y+shift) sw) ++ (assembleMap (x+shift,y+shift) se)

--Converts a grid to a QCell using the helper function and by keeping track of the 'starting location' (top left corner)
gridToQCell :: Grid -> QCell
gridToQCell g = gridToCellHelp (0,0) g 

gridToCellHelp :: Loc -> Grid -> QCell
gridToCellHelp l (G 1 m) = case (m Map.!? l) of
  Just x -> (C x)
  Nothing -> (C 0)
gridToCellHelp l@(x,y) (G n m) = let
  shift = n `div` 2
  h = logI n 
  g' = G shift m in 
    Q h (gridToCellHelp l g') (gridToCellHelp (x+shift,y) g') (gridToCellHelp (x,y+shift) g') (gridToCellHelp (x+shift,y+shift) g')

--Makes finding the log and converting it to an int less annoying to type out in the function
logI :: Int -> Int
logI = floor . logBase 2 . fromIntegral

--Tests the conversion functions
testTransform :: QCell -> Property
testTransform q = property $ gridToQCell (qcellToGrid q) == q

--Shrinks a given grid to the space made from a run of the hashlife algorithm
shrinkGrid :: Grid -> Grid
shrinkGrid (G s m) = let shift = s `div` 4 in G (s `div` 2) (Map.fromList (map (augment m shift) [(y,x) | x <- [shift..s-(shift+1)], y <- [shift..s-(shift+1)]])) where
  augment :: Map Loc Int -> Int -> Loc -> (Loc,Int)
  augment m shift (x,y) = case m Map.!? (x,y) of 
    Just l -> ((x-shift,y-shift), l)
    _ -> ((x-shift,y-shift), 0)

--Advances a grid n number of times
advance :: Int -> Grid -> Grid
advance 0 g = g
advance n g = advance (n-1) (nextGen g)

--Tests the algorithm using the many helper functions above
testHashlife :: QCell -> Property
testHashlife q@(Q n nw ne sw se) = n > 1 ==> property $ evalNextGenM q == gridToQCell (shrinkGrid (advance (2^(n-2)) (qcellToGrid q)))



{- These are some functions that I used while testing the hashlife algorithm. -}

--Returns an empty (all 0s) QCell of size n
empty :: Int -> QCell
empty 0 = (C 0)
empty n = Q n (empty (n-1)) (empty (n-1)) (empty (n-1)) (empty (n-1)) 

--Live cell
lc :: QCell
lc = C 1

--Dead cell
dc :: QCell
dc = C 0

--Sample size 1 QCells
cell1 :: QCell
cell1 = Q 1 lc lc dc lc

cell2 :: QCell
cell2 = Q 1 dc dc dc dc

cell3 :: QCell
cell3 = Q 1 dc lc dc dc

cell4 :: QCell
cell4 = Q 1 lc lc lc dc

--Expands a given QCell to a QCell one size larger. Running hashlife on a expanded QCell will return the space of the initial QCell
expand :: QCell -> QCell
expand (Q n ne nw se sw) = Q (n+1) (Q n (empty (n-1)) (empty (n-1)) (empty (n-1)) ne) (Q n (empty (n-1)) (empty (n-1)) nw (empty (n-1))) (Q n (empty (n-1)) se (empty (n-1)) (empty (n-1))) (Q n sw (empty (n-1)) (empty (n-1)) (empty (n-1)))
expand _ = error "No expand for unit cell"



{- Below is the hashlife algorithm itself and its helper functions. 
   I learned how the algorithm worked from this link: https://www.drdobbs.com/jvm/an-algorithm-for-compressing-space-and-t/184406478
   Basically, the main idea of the algorithm is that the next generation function of a quadtree tree is a recursive function that recurses on subtrees; and that the result of each run of the 
   next generation function is stored in a hashtable. Here, I take the algorithm and apply the memo monad to make the code more elegant in haskell. The memoized hashlife algorithm stores a map from
   quadcells to their successors, such that if a duplicate quadcell is encountered you do not need to recompute its sucessor. -}

--Given a list of QCell neighbors, sum the number of live neighbors
sumN :: [QCell] -> Int
sumN (c:cs) = case c of
  (C x) -> x + sumN cs
  _ -> error "No sum neighbors for non-unit cells"
sumN [] = 0

--Determines if a cell lives or dies given its neighbors (Judge, Jury and Executioner)
dredd :: QCell -> [QCell] -> QCell
dredd (C 0) cs | (sumN cs) == 3 = (C 1)
dredd (C 1) cs | (sumN cs) == 2 || (sumN cs) == 3 = (C 1)
dredd (C _) _ = (C 0)
dredd _ _ = error "No dredd for non-unit cells" 

--Horizontal, vertical and centered subsections of a given QCell of arbitrary size
horizontal :: QCell -> QCell -> QCell
horizontal (Q n _ wne _ wse) (Q _ enw _ esw _) = (Q n wne enw wse esw)
horizontal _ _ = error "Invalid input to horizontal"

vertical :: QCell -> QCell -> QCell
vertical (Q n _ _ nsw nse) (Q _ snw sne _ _) = (Q n nsw nse snw sne)
vertical _ _ = error "Invalid input to horizontal"

centered :: QCell -> QCell -> QCell -> QCell -> QCell
centered (Q n _ _ _ nwse) (Q _ _ _ nesw _) (Q _ _ swne _ _) (Q _ senw _ _ _) = Q n nwse nesw swne senw

--The hashlife algorithm. Calculates next gen for h=2 cells using the normal iterative method, and the fancy recursive version for larger cells
nextGenM :: (MonadMemo QCell QCell m) => QCell -> m QCell
nextGenM (Q 2 (Q 1 nw1 ne1 sw1 se1) (Q 1 nw2 ne2 sw2 se2) (Q 1 nw3 ne3 sw3 se3) (Q 1 nw4 ne4 sw4 se4)) = 
  return $ Q 1 (dredd se1 [nw1,ne1,nw2,sw2,nw4,ne3,nw3,sw1]) (dredd sw2 [ne1,nw2,ne2,se2,ne4,nw4,ne3,se1]) (dredd ne3 [sw1,se1,sw2,nw4,sw4,se3,sw3,nw3]) (dredd nw4 [se1,sw2,se2,ne4,se4,sw4,se3,ne3])
nextGenM (Q n nw ne sw se) | n > 2 = do
  n00 <- memo nextGenM nw
  n01 <- memo nextGenM (horizontal nw ne)
  n02 <- memo nextGenM ne
  n10 <- memo nextGenM (vertical nw sw)
  n11 <- memo nextGenM (centered nw ne sw se)
  n12 <- memo nextGenM (vertical ne se)
  n20 <- memo nextGenM sw
  n21 <- memo nextGenM (horizontal sw se)
  n22 <- memo nextGenM se
  newNW <- memo nextGenM (Q (n-1) n00 n01 n10 n11)
  newNE <- memo nextGenM (Q (n-1) n01 n02 n11 n12)
  newSW <- memo nextGenM (Q (n-1) n10 n11 n20 n21)
  newSE <- memo nextGenM (Q (n-1) n11 n12 n21 n22)
  return $ Q (n-1) newNW newNE newSW newSE
nextGenM _ = error "No nextgen"

--Evaluates the result of the memoized algorithm
evalNextGenM = startEvalMemo . nextGenM
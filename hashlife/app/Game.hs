{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Game where

{- Kameron Hnath
   5/16/22
   CMSC488B Final Project -}
   
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Control.Lens hiding ((<|), (|>))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import System.Random (Random(..), newStdGen)
import Data.Maybe (fromMaybe, isJust, fromJust, isNothing)
import Data.List (nub)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ix(range)

-- Types: Loc is used to set up the board for the game of life. The game is defined by a board and a boolean determining if the game is done or not.
type Loc = (Int, Int)

type Board = Map Loc Bool

data Game = Game
  { _board :: Board
  , _done  :: Bool
  } deriving (Show)

makeLenses ''Game

-- Height and width define the size of the board

height, width :: Int
height = 32
width  = 40

--Initializes the board with all empty squares except for the ones defined in the list at the end. You can change this value to one of the starting patterns below.
initBoard :: Board
initBoard = foldr (\k m -> Map.insert k True m) (Map.fromList [((x,y),False) | x <- [0..width-1], y <- [0..height-1]]) cycle15 --Also try cycle15 or cycle3

-- Cool starting patterns
beacon :: [Loc]
beacon = [(1,2),(2,1),(1,1),(2,2),(3,3),(3,4),(4,3),(4,4)]

glider1 :: [Loc]
glider1 = [(12,11),(15,11),(11,12),(11,13),(11,14),(12,14),(13,14),(14,14),(14,13)]

toad :: [Loc]
toad = [(2,2),(3,2),(4,2),(1,3),(2,3),(3,3)]

cycle15 :: [Loc]
cycle15 = [(15,10),(14,11),(16,11),(17,12),(13,12),(13,13),(13,14),(13,15),(13,16),(13,17),(17,13),(17,14),(17,15),(17,16),(17,17),(16,18),(15,19),(14,18)]

loaf :: [Loc]
loaf = [(1,3),(2,4),(3,5),(4,4),(4,3),(3,2),(2,2)]

pres1 :: [Loc]
pres1 = [(12,11),(15,11),(11,12),(11,13),(11,14),(12,14),(13,14),(14,14),(14,13),(1,2),(2,1),(1,1),(2,2),(3,3),(3,4),(4,3),
  (4,4),(32,32),(33,32),(34,32),(31,33),(32,33),(33,33),(21,23),(22,24),(23,25),(24,24),(24,23),(23,22),(22,22)]

--Gospers glider gun
gosper :: [Loc]
gosper = [(1,15),(1,16),(2,15),(2,16),(11,15),(11,16),(11,17),(12,14),(12,18),(13,13),(13,19),(14,13),(14,19),(15,16),(16,14),(16,18),
  (17,15),(17,16),(17,17),(18,16),(21,13),(21,14),(21,15),(22,13),(22,14),(22,15),(23,12),(23,16),(25,11),(25,12),(25,16),(25,17),(35,13),(35,14),(36,13),(36,14)]

cycle3 :: [Loc]
cycle3 = [(13,15),(13,16),(13,17),(13,21),(13,22),(13,23),(15,13),(15,18),(15,20),(15,25),(16,13),(16,18),(16,20),(16,25),(17,13),
  (17,18),(17,20),(17,25),(18,15),(18,16),(18,17),(18,21),(18,22),(18,23),(20,15),(20,16),(20,17),(20,21),(20,22),(20,23),(21,13),(21,25),(22,13),(22,25),(23,13),
  (23,25),(25,15),(25,16),(25,17),(25,21),(25,22),(25,23),(21,18),(22,18),(23,18),(21,20),(22,20),(23,20)]


             
-- Step forward in time by applying the nextGen function
step :: Game -> Game
step g = g & board %~ nextGen


--Defines a set of neighbors based on the bounds of the board
neighbors :: Loc -> [(Loc)]
neighbors (0,0)                             = [(1,0),(0,1),(1,1)]
neighbors (0,y) | y==height-1               = [(1,y),(0,y-1),(1,y-1)]
neighbors (x,0) | x==width-1                = [(x-1,0),(x,1),(x-1,1)]
neighbors (x,y) | x==width-1 && y==height-1 = [(x-1,y),(x,y-1),(x-1,y-1)]
neighbors (0,y)                             = [(1,y),(0,y-1),(0,y+1),(1,y-1),(1,y+1)]
neighbors (x,y) | x==width-1                = [(x-1,y),(x,y-1),(x,y+1),(x-1,y-1),(x-1,y+1)]
neighbors (x,0)                             = [(x,1),(x-1,0),(x+1,0),(x-1,1),(x+1,1)]
neighbors (x,y) | y==height-1               = [(x,y-1),(x-1,y),(x+1,y),(x+1,y-1),(x-1,y-1)]
neighbors (x,y)                             = [(x+1,y),(x-1,y),(x,y-1),(x,y+1),(x-1,y+1),(x+1,y+1),(x-1,y-1),(x+1,y-1)]

--Checks how many neighbors of a given cell are alive
liveNeighbors :: Board -> Loc -> Int
liveNeighbors board loc = foldr (checkLoc board) 0 (neighbors loc)

--Checks if a specific location is alive or not
checkLoc :: Board -> Loc -> Int -> Int
checkLoc board loc count = case (Map.lookup loc board) of 
  Just x -> if x then count + 1 else count
  _ -> count

--Checks if a cell is alive in the next generation or not
stateNextGen :: Board -> Loc -> Bool
stateNextGen board loc = case Map.lookup loc board of
  Just True -> (liveNeighbors board loc) == 3 || (liveNeighbors board loc) == 2
  _ -> (liveNeighbors board loc) == 3

--Change the board to the next generation
nextGen :: Board -> Board
nextGen board = Map.fromList $ map (\l -> (l,stateNextGen board l)) (Map.keys board)

-- Initialization
initGame :: IO Game
initGame = 
  return $ Game { _board = initBoard
                , _done = False }
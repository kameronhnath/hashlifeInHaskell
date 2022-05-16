{-# LANGUAGE OverloadedStrings #-}
module UI where

{- Kameron Hnath
   5/16/22
   CMSC488B Final Project -}
   
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO)

import Game
import Hashlife

import Brick
  ( App(..), AttrMap, BrickEvent(..), EventM, Next, Widget
  , customMain, neverShowCursor
  , continue, halt
  , hLimit, vLimit, vBox, hBox
  , padRight, padLeft, padTop, padAll, Padding(..)
  , withBorderStyle
  , str
  , attrMap, withAttr, emptyWidget, AttrName, on, fg, bg
  , (<+>)
  )
import Brick.BChan (newBChan, writeBChan)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.), at)
import qualified Graphics.Vty as V

import Data.Maybe (fromMaybe)
import Data.List  (intersperse)
import Test.QuickCheck

-- I used the tic-tac-toe implementation that we did in class as a baseline for my UI code, and tweaked a few values and functions.

data Tick = Tick
type Name = ()

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

--The main function runs the UI, and once the user quits out, shows some results of quickCheck on the hashlife algorithm.
main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 150000 -- decides how fast your game moves
  g <- initGame
  let builder = V.mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g
  putStrLn "Testing the hashlife algorithm with input shown:"
  quickCheckWith stdArgs { maxSuccess = 5 } . verbose $  testHashlife
  putStrLn "Testing more cases for the hashlife algorithm:"
  quickCheck testHashlife


-- Handling events: the only events are stepping forward in time and quiting out

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = continue $ step g -- Step Time
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g            -- Quit
handleEvent g _                                     = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g = [vBox [ withBorderStyle BS.unicodeBold 
                   $ B.borderWithLabel (str "Game of Life")
                   $ drawGrid g]]

--Draws each cell as "  " with live cells being blue
drawGrid :: Game -> Widget Name
drawGrid g = vBox rows
  where
    rows    = [hBox $ cells y | y <- [0..height-1]]
    cells y = [drawCell x y   | x <- [0..width-1]]
    drawCell x y =
      case g ^. board . at (x,y) of
            Just True  -> (withAttr xAttr) $ str "  "
            _ -> id $ str "  "

--Live cells are represented by a blue square (or 2 blue spaces)
theMap :: AttrMap
theMap = attrMap V.defAttr [(xAttr, bg V.blue)]

xAttr :: AttrName
xAttr = "xAttr"
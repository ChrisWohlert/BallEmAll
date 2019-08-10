module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Lib
import Drawings
import Game
import Models
import Block
import Ball
import Bat
import Debug.Trace
import System.Random

window :: Display
window = InWindow "Hello" (1000, 700) (10, 10)

background :: Color
background = white

draw :: Game -> Picture
draw =  getPicture

main :: IO ()
main = do
  rand <- newRand
  play window background 60 (initialGame (map abs $ randomList rand)) draw onEvent onAnimate
    where
      onEvent :: Event -> Game -> Game
      onEvent (EventKey (SpecialKey KeyLeft) Down modifiers (x, y)) (Game blocks bat ball time settings state dbs) = Game blocks (moveLeft bat) ball time settings state dbs
      onEvent (EventKey (SpecialKey KeyRight) Down modifiers (x, y)) (Game blocks bat ball time settings state dbs) = Game blocks (moveRight bat) ball time settings state dbs
      onEvent (EventKey (SpecialKey KeyLeft) Up modifiers (x, y)) (Game blocks bat ball time settings state dbs) = Game blocks (stopMovingLeft bat) ball time settings state dbs
      onEvent (EventKey (SpecialKey KeyRight) Up modifiers (x, y)) (Game blocks bat ball time settings state dbs) = Game blocks (stopMovingRight bat) ball time settings state dbs
      onEvent _ game = game
      onAnimate :: Float -> Game -> Game
      onAnimate = updateGame

newRand = randomIO :: IO Int
randomList seed = randoms (mkStdGen seed) :: [Int]
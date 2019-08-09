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

window :: Display
window = InWindow "Hello" (1000, 700) (10, 10)

background :: Color
background = white

draw :: Game -> Picture
draw =  getPicture

main :: IO ()
main = play window background 60 initialGame draw onEvent onAnimate
  where
    onEvent :: Event -> Game -> Game
    onEvent (EventKey (SpecialKey KeyLeft) Down modifiers (x, y)) (Game blocks bat ball time settings state) = Game blocks (moveLeft bat) ball time settings state
    onEvent (EventKey (SpecialKey KeyRight) Down modifiers (x, y)) (Game blocks bat ball time settings state) = Game blocks (moveRight bat) ball time settings state
    onEvent (EventKey (SpecialKey KeyLeft) Up modifiers (x, y)) (Game blocks bat ball time settings state) = Game blocks (stopMovingLeft bat) ball time settings state
    onEvent (EventKey (SpecialKey KeyRight) Up modifiers (x, y)) (Game blocks bat ball time settings state) = Game blocks (stopMovingRight bat) ball time settings state
    onEvent _ game = game
    onAnimate :: Float -> Game -> Game
    onAnimate t (Game blocks bat Nothing time settings _) = Game blocks bat Nothing time settings GameOver
    onAnimate t (Game [] bat ball time settings _) = Game [] bat ball time settings GameWon
    onAnimate t (Game blocks bat (Just ball) time settings state) =
      if time > 5 then
        Game
          (destroyBlock ball blocks)
          (moveBat settings bat)
          ((destroyBall . moveBall . moveAngle (map toBlock blocks) bat . splitBall blocks) ball)
          (time + t)
          settings
          state
      else Game blocks bat (Just ball) (time + t) settings state

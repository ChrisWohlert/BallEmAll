module Game where

import Data.List
import Debug.Trace
import Data.Fixed
import Models
import Block
import Ball
import Bat
import Collision

initialGame :: [Int] -> Game
initialGame seed = Game (getBlocks 1000 600 300 seed) (Bat (Box 150 20) (500, 50) NotMoving) (Just (Ball (Sphere 5) (100, 150) 7 3 Nothing Nothing Nothing Nothing)) 0 (GameSettings 1000 700 seed) GameRunning []

addTime :: Float -> Game -> Game
addTime t (Game bs bat ball currentTime settings state dbs) = Game bs bat ball (currentTime + t) settings state dbs

updateGame :: Float -> Game -> Game
updateGame t (Game blocks bat Nothing time settings _ dbs) = Game blocks bat Nothing time settings GameOver dbs
updateGame t (Game [] bat ball time (GameSettings w h s) _ dbs) = Game (getBlocks w (h - 100) (h - 400) (tail s)) bat ball time (GameSettings w h s) GameRunning dbs
updateGame t (Game blocks bat (Just ball) time settings state destroyedBlocks) =
  let
    updatedBall = updateBall (onBallLoop . moveBall . moveAngle . powerUp . getBlockCollisions blocks . getWallCollision . getBatCollision bat) ball
    updatedBat = (onBatLoop . moveBat settings . powerUpBat updatedBall) bat
    remainingBlocks = destroyBlock updatedBall blocks
  in
    if time > 3 then
     Game
       remainingBlocks
       updatedBat
       (destroyBall updatedBall)
       (time + t)
       settings
       state
       ((blocks \\ remainingBlocks) ++ destroyedBlocks)
    else Game blocks bat (Just ball) (time + t) settings state destroyedBlocks

gameScore :: [Block] -> Float -> Score
gameScore dbs time = Score $ maximum [length dbs * 100 - round (time * 99), length dbs * 3]
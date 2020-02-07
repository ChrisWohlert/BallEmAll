module Game where

import Data.List
import Data.Fixed
import Models
import Block
import Ball
import Bat
import Collision

initialGame :: Game
initialGame = Game (getBlocks 321 1000 600 300) (Bat (Box 150 20) (500, 50) NotMoving) (Just (Ball (Sphere 5) (100, 150) 7 3 Nothing Nothing Nothing Nothing)) 0 (GameSettings 1000 700) GameRunning [] []

addTime :: Float -> Game -> Game
addTime t (Game bs bat ball currentTime settings state dbs his) = Game bs bat ball (currentTime + t) settings state dbs his

updateGame :: Float -> Game -> Game
updateGame t = updateGameState t . updateHistory

updateGameState t (Game blocks bat Nothing time settings _ dbs his) = Game blocks bat Nothing time settings GameOver dbs his
updateGameState t (Game [] bat ball time (GameSettings w h) _ dbs his) = Game (getBlocks (round time) w (h - 100) 300) bat ball time (GameSettings w h) GameRunning dbs his
updateGameState t (Game blocks bat (Just ball) time settings state destroyedBlocks his) =
  let
    updatedBall = updateBall (onBallLoop . moveBall . moveAngle . powerUp . getBlockCollisions blocks . getWallCollision . getBatCollision bat) ball
    updatedBat = (onBatLoop . moveBat settings . powerUpBat updatedBall) bat
    blocksToDestroy = destroyBlock updatedBall
    remainingBlocks = blocks \\ blocksToDestroy
  in
    if time > 3 then
     Game
       remainingBlocks
       updatedBat
       (destroyBall updatedBall)
       (time + t)
       settings
       state
       (blocksToDestroy ++ destroyedBlocks)
       his
    else Game blocks bat (Just ball) (time + t) settings state destroyedBlocks his

gameScore :: [Block] -> Float -> Score
gameScore dbs time = Score $ maximum [length dbs * 100 - round (time * 78), length dbs * 3]

updateHistory :: Game -> Game
updateHistory game = let Game blocks bat ball time settings state destroyedBlocks his = game in Game blocks bat ball time settings state destroyedBlocks (game:his)

updateReplay t (Replay (b:bs) (Game blocks (Bat shape pos d) ball time settings state dbs his)) = Replay bs (updateGame t (Game blocks (Bat shape b d) ball time settings state dbs his))
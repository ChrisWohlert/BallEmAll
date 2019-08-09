module Game where

import Data.List
import Debug.Trace
import Data.Fixed
import Models
import Block

initialGame :: Game
initialGame = Game (getBlocks 1000 600 300) (Bat (Box 150 20) (500, 50) NotMoving) (Just (Ball (Sphere 5) (100, 150) 8 (2))) 0 (GameSettings 1000 700) GameRunning

addTime :: Float -> Game -> Game
addTime t (Game bs bat ball currentTime settings state) = Game bs bat ball (currentTime + t) settings state

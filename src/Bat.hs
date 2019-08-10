module Bat ( moveLeft
           , moveRight
           , stopMovingLeft
           , stopMovingRight
           , moveBat
           , powerUpBat
           , onBatLoop) where

import Models

moveLeft :: Bat -> Bat
moveLeft (Bat shape pos _) = Bat shape pos MovingLeft

moveRight :: Bat -> Bat
moveRight (Bat shape pos _) = Bat shape pos MovingRight

stopMovingLeft :: Bat -> Bat
stopMovingLeft (Bat shape pos MovingLeft) = Bat shape pos NotMoving
stopMovingLeft d = d

stopMovingRight :: Bat -> Bat
stopMovingRight (Bat shape pos MovingRight) = Bat shape pos NotMoving
stopMovingRight d = d

moveBat :: GameSettings -> Bat -> Bat
moveBat _ (Bat (Box boxW boxH) (x, y) MovingLeft) = Bat (Box boxW boxH) (if x - boxW / 2 > 0 then x - 10 else x, y) MovingLeft
moveBat (GameSettings w h) (Bat (Box boxW boxH) (x, y) MovingRight) = Bat (Box boxW boxH) (if x + boxW / 2 < w then x + 10 else x, y) MovingRight
moveBat _ bat = bat

onBatLoop :: Bat -> Bat
onBatLoop (Bat (Box w h) pos d) = Bat (Box (maximum [w - 0.01, 50]) h) pos d

powerUpBat :: Ball -> Bat -> Bat
powerUpBat ball bat =
  case ball of
    (Ball _ _ _ _ (Just (BlockCollision _ (Block _ _ (Just (BlockBatPowerUp f))))) _ _ _) -> f bat
    _ -> bat
module Drawings where

import Graphics.Gloss
import Game
import Models

class Drawable a where
  getPicture :: a -> Picture

instance Drawable Block where
  getPicture (Block (Box w h) (x, y)) = translate (x - 2) (y - 2) $ pictures [rectangleSolid (w + 4) (h + 4), Color (mixColors y x (makeColorI 45 78 161 255) (makeColorI 247 116 195 255)) $ rectangleSolid w h]
  getPicture (PowerUpBlock _ block) = Color (makeColorI 235 68 101 255) $ getPicture block

instance Drawable Bat where
  getPicture (Bat (Box w h) (x, y) _) = translate x y $ rectangleSolid w h

instance Drawable Ball where
  getPicture (Ball (Sphere w) (x, y) _ _) = translate x y $ Color (makeColorI 158 50 33 255) $ ThickCircle w (w * 2)
  getPicture (MultiBall balls) = pictures $ map getPicture balls
  getPicture (FireBall (Ball shape (x, y) vx vy) r) = pictures [getPicture (Ball shape (x, y) vx vy), translate x y $ Color (makeColorI 158 50 33 50) $ ThickCircle r (r * 2)]

instance Drawable GameState where
  getPicture GameRunning = blank
  getPicture GameWon = translate 100 300 $ pictures [Color white $ rectangleSolid 2000 250, Text "You've won!"]
  getPicture GameOver = translate 100 300 $ pictures [Color white $ rectangleSolid 2000 250, Text "GAME OVER!"]

instance (Drawable a) => Drawable (Maybe a) where
  getPicture (Just x) = getPicture x
  getPicture Nothing = blank

instance Drawable Game where
  getPicture (Game bs bat ball _ settings gameState) = translate (-500) (-350) $ pictures $ reverse $ getPicture gameState : getPicture bat : getPicture ball : map getPicture bs
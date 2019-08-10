module Drawings where

import Graphics.Gloss
import Game
import Models

class Drawable a where
  getPicture :: a -> Picture

instance Drawable Block where
  getPicture (Block (Box w h) (x, y) Nothing) = translate (x - 1) (y - 1) $ pictures [Color (makeColorI 0 0 0 180) $ rectangleSolid (w + 2) (h + 2), Color (mixColors y x (makeColorI 45 78 161 255) (makeColorI 247 116 195 255)) $ rectangleSolid w h]
  getPicture (Block (Box w h) (x, y) _) = translate (x - 1) (y - 1) $ pictures [Color (makeColorI 0 0 0 255) $ rectangleSolid (w + 2) (h + 2), Color (mixColors 1 1 (makeColorI 255 30 30 255) (makeColorI 18 30 30 255)) $ rectangleSolid w h]

instance Drawable Bat where
  getPicture (Bat (Box w h) (x, y) _) = translate x y $ rectangleSolid w h

instance Drawable Ball where
  getPicture (Ball (Sphere w) (x, y) _ _ _ _ _ Nothing) = translate x y $ Color (makeColorI 158 50 33 255) $ ThickCircle w (w * 2)
  getPicture (Ball _ _ _ _ _ _ _ (Just (MultiBall balls))) = pictures $ map getPicture balls
  getPicture (Ball shape (x, y) vx vy bc batc wc (Just (FireBall r _))) = pictures [getPicture (Ball shape (x, y) vx vy bc batc wc Nothing), translate x y $ Color (makeColorI 158 50 33 50) $ ThickCircle r (r * 2)]

instance Drawable GameState where
  getPicture GameRunning = blank
  getPicture GameWon = translate 100 300 $ pictures [Color white $ rectangleSolid 2000 250, Text "You've won!"]
  getPicture GameOver = translate 100 300 $ pictures [Color white $ rectangleSolid 2000 250, Text "GAME OVER!"]

instance (Drawable a) => Drawable (Maybe a) where
  getPicture (Just x) = getPicture x
  getPicture Nothing = blank

instance Drawable Score where
  getPicture (Score x) = translate 10 10 $ scale 0.2 0.2 $ Text (show x)

instance Drawable Game where
  getPicture (Game bs bat ball time settings gameState dbs _) = translate (-500) (-350) $ pictures $ reverse $ getPicture (gameScore dbs time) : getPicture gameState : getPicture bat : getPicture ball : map getPicture bs
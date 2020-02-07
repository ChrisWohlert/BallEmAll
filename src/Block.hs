module Block ( getBlocks
             , destroyBlock) where

import Models
import Collision
import Data.List
import Ball

getBlocks :: Int -> Float -> Float -> Float -> [Block]
getBlocks seed width height floor = zipWith makePowerUpBlock
                                   (takeWhile isWithinWidth
                                      (iterate getNext (Block (Box 60 25) (50, height) Nothing))
                                      ++
                                      takeWhile isWithinFloor
                                        (getBlocks (seed * 32) width (height - 30) floor))
                                        [seed ..]
  where
    getNext (Block (Box w h) (x, y) pu) = Block (Box w h) (x + w + 5, y) pu
    isWithinWidth (Block _ (x, _) _) = x < width
    isWithinFloor (Block _ (_, y) _) = y > floor
    makePowerUpBlock :: Block -> Int -> Block
    makePowerUpBlock (Block shape pos pu) i
      | i `mod` 41 == 0 = Block shape pos $ Just $ BlockBallPowerUp makeNewFireBall
      | i `mod` 54 == 0 = Block shape pos $ Just $ BlockBallPowerUp makeNewBalls
      | i `mod` 38 == 0 = Block shape pos $ Just $ BlockBallPowerUp speedUp
      | i `mod` 35 == 0 = Block shape pos $ Just $ BlockBatPowerUp widenBat
      | otherwise = Block shape pos pu
        where
          makeNewFireBall (Ball shape pos vx vy bc batc wc (Just (FireBall r bs))) = updatePowerUp (Just (FireBall (maximum [r + 20, 50]) [])) (Ball shape pos vx vy bc batc wc (Just (FireBall r bs)))
          makeNewFireBall ball = updatePowerUp (Just (FireBall 50 [])) ball
          makeNewBalls (Ball shape pos vx vy bc batc wc pu) = Ball shape pos vx vy bc batc wc $ Just $ MultiBall [Ball shape pos vx vy bc batc wc pu, Ball shape pos (vx * (-1)) (vy * (-1)) bc batc wc pu]
          speedUp (Ball shape pos vx vy bc batc wc pu) = Ball shape pos (vx * 1.05) (vy * 1.05) bc batc wc pu
          widenBat (Bat (Box w h) pos d) = Bat (Box (w + 30) h) pos d

destroyBlock :: Ball -> [Block]
destroyBlock (Ball _ _ _ _ (Just (BlockCollision _ block)) _ _ (Just (MultiBall balls))) = concatMap destroyBlock balls
destroyBlock (Ball _ _ _ _ (Just (BlockCollision _ block)) _ _ (Just (FireBall _ bcs))) = block:bcs
destroyBlock (Ball _ _ _ _ (Just (BlockCollision _ block)) _ _ _) = [block]
destroyBlock _ = []
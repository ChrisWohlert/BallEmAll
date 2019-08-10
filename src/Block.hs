module Block ( getBlocks
             , destroyBlock) where

import Models
import Collision
import Data.List
import Debug.Trace
import Ball

getBlocks :: Float -> Float -> Float -> [Int] -> [Block]
getBlocks width height floor (seed:seeds)= zipWith makePowerUpBlock
                                   (takeWhile isWithinWidth
                                      (iterate getNext (Block (Box 60 25) (50, height) Nothing))
                                      ++
                                      takeWhile isWithinFloor
                                        (getBlocks width (height - 30) floor seeds))
                                        (seed:seeds)
  where
    getNext (Block (Box w h) (x, y) pu) = Block (Box w h) (x + w + 5, y) pu
    isWithinWidth (Block _ (x, _) _) = x < width
    isWithinFloor (Block _ (_, y) _) = y > floor
    makePowerUpBlock :: Block -> Int -> Block
    makePowerUpBlock (Block shape pos pu) i
      | i `mod` 53 == 0 = trace (show (i `mod` 50)) Block shape pos $ Just $ BlockBallPowerUp makeNewFireBall
      | i `mod` 351 == 0 = Block shape pos $ Just $ BlockBallPowerUp makeNewBalls
      | i `mod` 35 == 0 = Block shape pos $ Just $ BlockBallPowerUp speedUp
      | i `mod` 40 == 0 = Block shape pos $ Just $ BlockBatPowerUp widenBat
      | otherwise = trace (show (i `mod` 50)) Block shape pos pu
        where
          makeNewFireBall (Ball shape pos vx vy bc batc wc (Just (FireBall r bs))) = updatePowerUp (Just (FireBall (maximum [r + 20, 50]) [])) (Ball shape pos vx vy bc batc wc (Just (FireBall r bs)))
          makeNewFireBall ball = updatePowerUp (Just (FireBall 50 [])) ball
          makeNewBalls (Ball shape pos vx vy bc batc wc pu) = Ball shape pos vx vy bc batc wc $ Just $ MultiBall [Ball shape pos vx vy bc batc wc pu, Ball shape pos (vx * (-1)) (vy * (-1)) bc batc wc pu]
          speedUp (Ball shape pos vx vy bc batc wc pu) = Ball shape pos (vx * 1.05) (vy * 1.05) bc batc wc pu
          widenBat (Bat (Box w h) pos d) = Bat (Box (w + 30) h) pos d

destroyBlock :: Ball -> [Block] -> [Block]
destroyBlock (Ball _ _ _ _ (Just (BlockCollision _ block)) _ _ (Just (MultiBall balls))) bs = foldl intersect bs $ map (`destroyBlock` bs) balls
destroyBlock (Ball _ _ _ _ (Just (BlockCollision _ block)) _ _ (Just (FireBall _ bcs))) bs = bs \\ (block:bcs)
destroyBlock (Ball _ _ _ _ (Just (BlockCollision _ block)) _ _ _) bs = bs \\ [block]
destroyBlock _ bs = bs
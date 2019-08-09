module Block ( getBlocks
             , destroyBlock
             , toBlock) where

import Models
import Collision
import Data.List
import Debug.Trace

toBlock :: Block -> Block
toBlock (PowerUpBlock _ block) = block
toBlock b = b

getBlocks :: Float -> Float -> Float -> [Block]
getBlocks width height floor = map (\(b, i) -> makePowerUpBlock b i) $ zip ((takeWhile isWithinWidth $ iterate (getNext) $ (Block (Box 45 15) (50, height))) ++ (takeWhile isWithinFloor $ map toBlock $ getBlocks width (height - 20) floor)) [0..]
  where
    getNext (Block (Box w h) (x, y)) = Block (Box w h) (x + w + 5, y)
    isWithinWidth (Block _ (x, _)) = x < width
    isWithinFloor (Block _ (_, y)) = y > floor
    makePowerUpBlock :: Block -> Int -> Block
    makePowerUpBlock block i
      | i > 0 && i `mod` 53 == 0 = PowerUpBlock (BallPowerUp (\ ball -> FireBall ball 50)) block
      | i > 0 && i `mod` 41 == 0 = PowerUpBlock (BallPowerUp (\ ball -> MultiBall [ball, Ball (ballShape ball) (ballPosition ball) ((ballSpeedX ball) * (-1)) ((ballSpeedY ball) * (-1))])) block
      | i > 0 && i `mod` 10 == 0 = PowerUpBlock (BallPowerUp speedUp) block
      | otherwise = block
        where
          speedUp (MultiBall balls) = MultiBall $ map speedUp balls
          speedUp (FireBall ball r) = FireBall (speedUp ball) r
          speedUp ball = Ball (ballShape ball) (ballPosition ball) (ballSpeedX ball + 1) (ballSpeedY ball + 1)

destroyBlock :: Ball -> [Block] -> [Block]
destroyBlock (MultiBall balls) bs = foldl intersect bs $ map (\ b -> destroyBlock b bs) balls
destroyBlock (FireBall (Ball shape (x ,y) vx vy) r) bs =
  let hasCollision = not $ null $ concat $ map (collidesWithBlock (Ball shape (x, y) vx vy)) bs
      distances = map getDistance bs
  in
      if hasCollision then bs \\ (map fst $ filter ((<= r * 2) . snd) distances) else bs
      where
        getDistance :: Block -> (Block, Float)
        getDistance (Block shape (bx, by)) = ((Block shape (bx, by)), sqrt $ (x - bx) ** 2 + (y - by) ** 2)
        getDistance (PowerUpBlock pu b) = let (_, d) = getDistance b in ((PowerUpBlock pu b), d)

destroyBlock ball bs = filter (null . collidesWithBlock ball) bs
module Ball ( moveBall
            , moveAngle
            , splitBall
            , destroyBall) where

import Models
import Data.Maybe
import Collision
import Debug.Trace

moveBall :: Ball -> Ball
moveBall (Ball shape (x, y) vx vy) = Ball shape (x + vx, y + vy) vx vy
moveBall (MultiBall balls) = MultiBall $ map moveBall balls
moveBall (FireBall ball r) = FireBall (moveBall ball) r

moveAngle :: [Block] -> Bat -> Ball -> Ball
moveAngle bs bat (MultiBall balls) = MultiBall $ map (moveAngle bs bat) balls
moveAngle bs bat (FireBall ball r) = FireBall (moveAngle bs bat ball) r
moveAngle bs bat ball = newBall
  where
    newBall = foldr changeCourse ball $ concat $ collidesWithBat ball bat : collidesWithWall ball : map (map fst . (collidesWithBlock ball)) bs

changeCourse :: Collision -> Ball -> Ball
changeCourse (Colliding CollisionRight) ball = Ball (ballShape ball) (ballPosition ball) (abs $ ballSpeedX ball) (ballSpeedY ball)
changeCourse (Colliding CollisionLeft) ball = Ball (ballShape ball) (ballPosition ball) (-1 * (abs $ ballSpeedX ball)) (ballSpeedY ball)
changeCourse (Colliding CollisionTop) ball = Ball (ballShape ball) (ballPosition ball) (ballSpeedX ball) (abs $ ballSpeedY ball)
changeCourse (Colliding CollisionBottom) ball = Ball (ballShape ball) (ballPosition ball) (ballSpeedX ball) ((-1 * (abs $ ballSpeedY ball)))

splitBall :: [Block] -> Ball -> Ball
splitBall bs (MultiBall balls) = MultiBall $ map (splitBall bs) balls
splitBall bs ball = case concat $ map (collidesWithBlock ball) bs of
                    [] -> ball
                    ((_, PowerUpBlock (BallPowerUp f) _):_) -> trace ("Hit powerup: " ++ (show $ f ball)) $ f ball
                    (_:_) -> trace "Hit normal" ball

destroyBall :: Ball -> Maybe Ball
destroyBall (MultiBall balls)
  | null ballsLeft = Nothing
  | otherwise = Just $ MultiBall ballsLeft
    where
      ballsLeft = mapMaybe destroyBall balls

destroyBall (Ball (Sphere r) (x, y) vx vy)
  | y - r <= 0= Nothing
  | otherwise = Just $ Ball (Sphere r) (x, y) vx vy

destroyBall (FireBall ball r) =
  case destroyBall ball of
  Nothing -> Nothing
  Just b -> Just $ FireBall b r

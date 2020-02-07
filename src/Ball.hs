module Ball ( moveBall
            , moveAngle
            , powerUp
            , destroyBall
            , getBlockCollisions
            , updatePowerUp
            , getWallCollision
            , getBatCollision
            , updateBall
            , onBallLoop) where

import Models
import Data.Maybe
import Collision
import Data.List
import Data.Monoid
import Util

updateBall :: (Ball -> Ball) -> Ball -> Ball
updateBall f (Ball shape pos vx vy bc batc wc(Just (MultiBall balls))) = Ball shape pos vx vy bc batc wc (Just (MultiBall (map (updateBall f) balls)))
updateBall f ball = f ball

moveBall :: Ball -> Ball
moveBall (Ball shape (x, y) vx vy bc batc wc pu) = Ball shape (x + vx, y + vy) vx vy bc batc wc pu

moveAngle :: Ball -> Ball
moveAngle = changeCourse

changeCourse :: Ball -> Ball
changeCourse ball = case ball of
  (Ball _ _ _ _ (Just (BlockCollision CollisionRight _)) _ _ _) -> ballMoveRight ball
  (Ball _ _ _ _ (Just (BlockCollision CollisionLeft _)) _ _ _) -> ballMoveLeft ball
  (Ball _ _ _ _ (Just (BlockCollision CollisionTop _)) _ _ _) -> ballMoveUp ball
  (Ball _ _ _ _ (Just (BlockCollision CollisionBottom _)) _ _ _) -> ballMoveDown ball
  (Ball _ _ _ _ _ (Just (BatCollision CollisionRight bat)) _ _) -> ballMoveRight ball
  (Ball _ _ _ _ _ (Just (BatCollision CollisionLeft bat)) _ _) -> ballMoveLeft ball
  (Ball _ _ _ _ _ (Just (BatCollision CollisionTop bat)) _ _) -> (addTilt bat . ballMoveUp) ball
  (Ball _ _ _ _ _ (Just (BatCollision CollisionBottom bat)) _ _) -> ballMoveDown ball
  (Ball _ _ _ _ _ _ (Just (WallCollision CollisionRight)) _) -> ballMoveLeft ball
  (Ball _ _ _ _ _ _ (Just (WallCollision CollisionLeft)) _) -> ballMoveRight ball
  (Ball _ _ _ _ _ _ (Just (WallCollision CollisionTop)) _) -> ballMoveDown ball
  (Ball _ _ _ _ _ _ (Just (WallCollision CollisionBottom)) _) -> ballMoveUp ball
  _ -> ball


ballMoveRight (Ball shape pos vx vy bc batc wc pu) = Ball shape pos (abs vx) vy bc batc wc pu
ballMoveLeft (Ball shape pos vx vy bc batc wc pu) = Ball shape pos (-1 * abs vx) vy bc batc wc pu
ballMoveDown (Ball shape pos vx vy bc batc wc pu) = Ball shape pos vx (-1 * abs vy) bc batc wc pu
ballMoveUp (Ball shape pos vx vy bc batc wc pu) = Ball shape pos vx (abs vy) bc batc wc pu

addTilt :: Bat -> Ball -> Ball
addTilt (Bat (Box w h) (x, y) _) (Ball shape (ballX, ballY) vx vy bc batc wc pu) = Ball shape (ballX, ballY) ((if x < ballX then (-) else (+)) vx ((x - ballX - w) / (w / 3))) vy bc batc wc pu

powerUp :: Ball -> Ball
powerUp ball = case ball of
                    (Ball _ _ _ _ (Just (BlockCollision _ (Block _ _ (Just (BlockBallPowerUp f))))) _ _ _) -> f ball
                    _ -> ball

destroyBall :: Ball -> Maybe Ball
destroyBall (Ball shape pos vx vy bc batc wc (Just (MultiBall balls)))
  | null ballsLeft = Nothing
  | otherwise = Just $ Ball shape pos vx vy bc batc wc (Just (MultiBall ballsLeft))
    where
      ballsLeft = mapMaybe destroyBall balls
destroyBall (Ball _ _ _ _ _ _ (Just (WallCollision CollisionBottom)) _) = Nothing
destroyBall ball = Just ball

getBlockCollisions :: [Block] -> Ball -> Ball

getBlockCollisions blocks (Ball shape pos vx vy bc batc wc (Just (MultiBall balls))) = Ball shape pos vx vy bc batc wc (Just (MultiBall $ map (getBlockCollisions blocks) balls))
getBlockCollisions blocks (Ball shape (x, y) vx vy bc batc wc (Just (FireBall r bcs))) =
  let
     distances = map (getDistance (x, y)) blocks
     fireball = Just $ FireBall r $ map fst (filter ((<= r * 2) . snd) distances)
  in
    updatePowerUp fireball (getBlockCollisions blocks (Ball shape (x, y) vx vy bc batc wc Nothing))
    where
      getDistance :: (Float, Float) -> Block -> (Block, Float)
      getDistance (x, y) (Block shape (bx, by) pu) = (Block shape (bx, by) pu, sqrt $ (x - bx) ** 2 + (y - by) ** 2)
getBlockCollisions blocks ball = let (Ball shape pos vx vy _ batc wc pu) = ball in Ball shape pos vx vy (collisionsWithBlock ball blocks) batc wc pu

getWallCollision :: Ball -> Ball
getWallCollision ball = let (Ball shape pos vx vy bc batc _ pu) = ball in Ball shape pos vx vy bc batc (collidesWithWall ball) pu

getBatCollision :: Bat -> Ball -> Ball
getBatCollision bat ball = let (Ball shape pos vx vy bc _ wc pu) = ball in Ball shape pos vx vy bc (collidesWithBat bat ball) wc pu

updatePowerUp :: Maybe BallPowerUp -> Ball -> Ball
updatePowerUp pu (Ball shape pos vx vy bc batc wc _) = Ball shape pos vx vy bc batc wc pu

onBallLoop :: Ball -> Ball
onBallLoop (Ball shape pos vx vy bc batc wc (Just (FireBall r fbs))) = Ball shape pos vx vy bc batc wc (Just (FireBall (maximum [r - 0.1, 0]) fbs))
onBallLoop ball = ball
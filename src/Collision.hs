module Collision ( collidesWithBat
                 , collidesWithBlock
                 , collidesWithWall
                 , collisionsWithBlock) where

import Models
import Util

collidesWithBat :: Bat -> Ball -> Maybe BatCollision
collidesWithBat (Bat shape pos d) ball = batCollision $ collidesWithBox ball shape pos
  where
    batCollision (Just CollisionTop) = Just $ BatCollision CollisionTop (Bat shape pos d)
    batCollision (Just CollisionBottom)= Just $ BatCollision CollisionBottom (Bat shape pos d)
    batCollision (Just CollisionRight) = Just $ BatCollision CollisionRight (Bat shape pos d)
    batCollision (Just CollisionLeft) = Just $ BatCollision CollisionLeft (Bat shape pos d)
    batCollision Nothing = Nothing


collidesWithBlock :: Block -> Ball -> Maybe BlockCollision
collidesWithBlock (Block shape pos pu) ball = blockCollision (Block shape pos pu) (collidesWithBox ball shape pos)
  where
    blockCollision :: Block -> Maybe Collision -> Maybe BlockCollision
    blockCollision block (Just CollisionTop) = Just $ BlockCollision CollisionTop block
    blockCollision block (Just CollisionBottom) = Just $ BlockCollision CollisionBottom block
    blockCollision block (Just CollisionRight) = Just $ BlockCollision CollisionRight block
    blockCollision block (Just CollisionLeft) = Just $ BlockCollision CollisionLeft block
    blockCollision _ Nothing = Nothing

collidesWithBox :: Ball -> Shape -> Position -> Maybe Collision
collidesWithBox (Ball (Sphere r) (x, y) vx vy _ _ _ _) (Box w h) (bx, by)
  | topBall >= bottom && topBall <= bottom + r + abs vy && rightBall >= left && leftBall <= right = Just CollisionBottom
  | bottomBall <= top && bottomBall >= top - r - abs vy && leftBall <= right && rightBall >= left = Just CollisionTop
  | rightBall >= left && rightBall <= left + r + abs vx && bottomBall <= top && topBall >= bottom = Just CollisionLeft
  | leftBall <= right && leftBall >= right - r - abs vx && bottomBall <= top && topBall >= bottom = Just CollisionRight
  | otherwise = Nothing
    where
      bottom = by - h / 2
      top = by + h / 2
      left = bx - w / 2
      right = bx + w / 2
      topBall = y + r
      bottomBall = y - r
      leftBall = x - r
      rightBall = x + r

collidesWithWall :: Ball -> Maybe WallCollision
collidesWithWall (Ball (Sphere r) (x, y) _ _ _ _ _ _)
  | x + r >= 1000 = Just $ WallCollision CollisionRight
  | x - r <= 0 = Just $ WallCollision CollisionLeft
  | y + r >= 700 = Just $ WallCollision CollisionTop
  | y - r <= 0 = Just $ WallCollision CollisionBottom
  | otherwise = Nothing

collisionsWithBlock :: Ball -> [Block] -> Maybe BlockCollision
collisionsWithBlock ball = maybeHead . map (`collidesWithBlock` ball)
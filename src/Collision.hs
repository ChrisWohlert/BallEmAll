module Collision ( collidesWithBat
                 , collidesWithBlock
                 , collidesWithWall) where

import Models

collidesWithBat :: Ball -> Bat -> [Collision]
collidesWithBat (FireBall ball _) bat = collidesWithBat ball bat
collidesWithBat ball (Bat shape pos _) = collidesWithBox ball shape pos

collidesWithBlock :: Ball -> Block -> [(Collision, Block)]
collidesWithBlock (FireBall ball _) block = collidesWithBlock ball block
collidesWithBlock ball (Block shape pos) = zip (collidesWithBox ball shape pos) (repeat (Block shape pos))
collidesWithBlock ball (PowerUpBlock pu block) = [(c, PowerUpBlock pu block) | (c, _) <- collidesWithBlock ball block]

collidesWithBox :: Ball -> Shape -> Position -> [Collision]
collidesWithBox (Ball (Sphere r) (x, y) vx vy) (Box w h) (bx, by) =
  map snd $ filter fst
  [ (topBall >= bottom && topBall <= bottom + r + abs vy && rightBall >= left && leftBall <= right, Colliding CollisionBottom)
  , (bottomBall <= top && bottomBall >= top - r - abs vy && leftBall <= right && rightBall >= left, Colliding CollisionTop)
  , (rightBall >= left && rightBall <= left + r + abs vx && bottomBall <= top && topBall >= bottom, Colliding CollisionLeft)
  , (leftBall <= right && leftBall >= right - r - abs vx && bottomBall <= top && topBall >= bottom, Colliding CollisionRight)
  ]
    where
      bottom = by - h / 2
      top = by + h / 2
      left = bx - w / 2
      right = bx + w / 2
      topBall = y + r
      bottomBall = y - r
      leftBall = x - r
      rightBall = x + r

collidesWithBox (FireBall ball _) shape pos = collidesWithBox ball shape pos

collidesWithWall :: Ball -> [Collision]
collidesWithWall (Ball (Sphere r) (x, y) _ _) =
  map snd $ filter fst
  [ (x + r >= 1000, Colliding CollisionLeft)
  , (x - r <= 0, Colliding CollisionRight)
  , (y + r >= 700, Colliding CollisionBottom)
  , (y - r <= 0, Colliding CollisionTop)
  ]

collidesWithWall (FireBall ball _) = collidesWithWall ball
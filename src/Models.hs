module Models where

type Position = (Float, Float)

type Height = Float
type Width = Float

data Shape = Box Width Height | Sphere Width deriving (Show, Eq)

data PowerUp = BallPowerUp (Ball -> Ball)

data Block = Block { blockShape :: Shape
                   , blockPosition :: Position
                   }
                   | PowerUpBlock PowerUp Block

data BatDirection = MovingLeft | NotMoving | MovingRight deriving (Show)

data Bat = Bat { batShape :: Shape
               , batPosition :: Position
               , batDirection :: BatDirection
               } deriving (Show)

data CollisionTarget = CollisionTop | CollisionBottom | CollisionLeft | CollisionRight

data Collision = NotColliding | Colliding CollisionTarget

data Ball = Ball { ballShape :: Shape
                 , ballPosition :: Position
                 , ballSpeedX :: Float
                 , ballSpeedY :: Float
                 }
                 | MultiBall [Ball]
                 | FireBall Ball Float deriving (Show, Eq)

data GameSettings = GameSettings { windowWidth :: Float
                                 , windowHeight :: Float
                                 } deriving (Show)

data GameState = GameRunning | GameWon | GameOver deriving (Show)

data Game = Game { blocks :: [Block]
                 , bat :: Bat
                 , ball :: Maybe Ball
                 , time :: Float
                 , settings :: GameSettings
                 , gameState :: GameState
                 } deriving (Show)

instance Eq Block where
  (Block shape pos) == (Block shape2 pos2) = shape == shape2 && pos == pos2
  (PowerUpBlock _ block) == (PowerUpBlock _ block2) = block == block2
  (PowerUpBlock _ block) == block2 = block == block2
  block == (PowerUpBlock _ block2) = block == block2

instance Show Block where
  show (PowerUpBlock _ block) = show block
  show (Block shape pos) = show shape ++ " " ++ show pos
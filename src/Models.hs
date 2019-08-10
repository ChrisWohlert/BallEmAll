module Models where

type Position = (Float, Float)

type Height = Float
type Width = Float

data Shape = Box Width Height | Sphere Width deriving (Show, Eq)

data BlockPowerUp = BlockBallPowerUp (Ball -> Ball) | BlockBatPowerUp (Bat -> Bat)

data BallPowerUp = MultiBall [Ball]
                 | FireBall Float [Block]
                 deriving (Show, Eq)

data Block = Block { blockShape :: Shape
                   , blockPosition :: Position
                   , blockPowerUp :: Maybe BlockPowerUp
                   }

data BatDirection = MovingLeft | NotMoving | MovingRight deriving (Show, Eq)

data Bat = Bat { batShape :: Shape
               , batPosition :: Position
               , batDirection :: BatDirection
               } deriving (Show, Eq)

data Collision = CollisionTop
               | CollisionBottom
               | CollisionLeft
               | CollisionRight
               deriving (Show, Eq)

data BlockCollision = BlockCollision Collision Block deriving (Show, Eq)

data BatCollision = BatCollision Collision Bat deriving (Show, Eq)

newtype WallCollision = WallCollision Collision deriving (Show, Eq)

data Ball = Ball { ballShape :: Shape
                 , ballPosition :: Position
                 , ballSpeedX :: Float
                 , ballSpeedY :: Float
                 , ballBlockCollisions :: Maybe BlockCollision
                 , ballBatCollision :: Maybe BatCollision
                 , ballWallCollision :: Maybe WallCollision
                 , ballPowerUp :: Maybe BallPowerUp
                 } deriving (Show, Eq)

data GameSettings = GameSettings { windowWidth :: Float
                                 , windowHeight :: Float
                                 } deriving (Show)

data GameState = GameRunning | GameWon | GameOver deriving (Show)

data Score = Score Int

data Game = Game { blocks :: [Block]
                 , bat :: Bat
                 , ball :: Maybe Ball
                 , time :: Float
                 , settings :: GameSettings
                 , gameState :: GameState
                 , destroyedBlocks :: [Block]
                 , history :: [Game]
                 } deriving (Show)

data Replay = Replay { replayBatPositions :: [Position], replayGame :: Game }

instance Eq Block where
  (Block shape pos _) == (Block shape2 pos2 _) = shape == shape2 && pos == pos2

instance Show Block where
  show (Block shape pos _) = show shape ++ " " ++ show pos
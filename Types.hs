module Types where

data BrickHP = One | Two | Three
    deriving (Eq, Show)

data Brick = Brick
    { brickX  :: Float
    , brickY  :: Float
    , brickHP :: BrickHP
    } deriving (Show)

data Paddle = Paddle
    { paddleX :: Float
    } deriving (Show)

data Ball = Ball
    { ballX  :: Float
    , ballY  :: Float
    , ballVx :: Float
    , ballVy :: Float
    } deriving (Show)

data Phase
    = WaitingToLaunch
    | Playing
    | LevelCleared
    | GameOver
    | Victory
    deriving (Eq, Show)

data GameState = GameState
    { gsBall   :: Ball
    , gsPaddle :: Paddle
    , gsBricks :: [Brick]
    , gsLives  :: Int
    , gsScore  :: Int
    , gsPhase  :: Phase
    } deriving (Show)

data Input = Input
    { inputLeft  :: Bool
    , inputRight :: Bool
    } deriving (Show)

data World = World
    { worldState :: GameState
    , worldInput :: Input
    } deriving (Show)

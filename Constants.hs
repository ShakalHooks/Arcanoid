module Constants where

windowWidth :: Int
windowWidth = 800

windowHeight :: Int
windowHeight = 600

windowTitle :: String
windowTitle = "Arkanoid"

fps :: Int
fps = 60

paddleWidth :: Float
paddleWidth = 100.0

paddleHeight :: Float
paddleHeight = 14.0

paddleY :: Float
paddleY = -250.0

paddleSpeed :: Float
paddleSpeed = 350.0

paddleHalfW :: Float
paddleHalfW = paddleWidth / 2.0

ballRadius :: Float
ballRadius = 8.0

ballInitialSpeed :: Float
ballInitialSpeed = 280.0

ballInitVx :: Float
ballInitVx = 200.0

ballInitVy :: Float
ballInitVy = ballInitialSpeed

brickWidth :: Float
brickWidth = 68.0

brickHeight :: Float
brickHeight = 22.0

brickCols :: Int
brickCols = 10

brickRows :: Int
brickRows = 5

brickMargin :: Float
brickMargin = 4.0

brickOffsetY :: Float
brickOffsetY = 80.0

brickOffsetX :: Float
brickOffsetX = 46.0

wallLeft :: Float
wallLeft = fromIntegral windowWidth / 2.0

wallTop :: Float
wallTop = fromIntegral windowHeight / 2.0

initialLives :: Int
initialLives = 3

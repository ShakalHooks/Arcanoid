module Logic
    ( initialWorld
    , stepWorld
    , handleEvent
    ) where

import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), SpecialKey(..), KeyState(..))

import Types
import Constants
initialBricks :: [Brick]
initialBricks =
    [ Brick bx by hp
    | row <- [0 .. brickRows - 1]
    , col <- [0 .. brickCols - 1]
    , let bx = brickOffsetX - wallLeft
                + fromIntegral col * (brickWidth + brickMargin)
                + brickWidth / 2.0
    , let by = wallTop - brickOffsetY
                - fromIntegral row * (brickHeight + brickMargin)
                - brickHeight / 2.0
    , let hp = rowToHP row
    ]

rowToHP :: Int -> BrickHP
rowToHP 0 = Three
rowToHP 1 = Three
rowToHP 2 = Two
rowToHP 3 = One
rowToHP _ = One

ballOnPaddle :: Paddle -> Ball
ballOnPaddle p = Ball
    { ballX  = paddleX p
    , ballY  = paddleY + paddleHeight / 2.0 + ballRadius
    , ballVx = ballInitVx
    , ballVy = ballInitVy
    }

initialState :: GameState
initialState =
    let paddle = Paddle 0.0
    in  GameState
            { gsBall   = ballOnPaddle paddle
            , gsPaddle = paddle
            , gsBricks = initialBricks
            , gsLives  = initialLives
            , gsScore  = 0
            , gsPhase  = WaitingToLaunch
            }

initialWorld :: World
initialWorld = World
    { worldState = initialState
    , worldInput = Input False False
    }
handleEvent :: Event -> World -> World
handleEvent (EventKey (SpecialKey KeyLeft)  Down _ _) w =
    w { worldInput = (worldInput w) { inputLeft  = True  } }
handleEvent (EventKey (SpecialKey KeyLeft)  Up   _ _) w =
    w { worldInput = (worldInput w) { inputLeft  = False } }
handleEvent (EventKey (SpecialKey KeyRight) Down _ _) w =
    w { worldInput = (worldInput w) { inputRight = True  } }
handleEvent (EventKey (SpecialKey KeyRight) Up   _ _) w =
    w { worldInput = (worldInput w) { inputRight = False } }
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) w =
    w { worldState = handleSpace (worldState w) }
handleEvent _ w = w

handleSpace :: GameState -> GameState
handleSpace gs = case gsPhase gs of
    WaitingToLaunch -> gs { gsPhase = Playing }
    GameOver        -> initialState
    Victory         -> initialState
    LevelCleared    -> initialState
    Playing         -> gs
stepWorld :: Float -> World -> World
stepWorld dt w = w { worldState = stepState dt (worldInput w) (worldState w) }

stepState :: Float -> Input -> GameState -> GameState
stepState _  _     gs@(GameState { gsPhase = GameOver     }) = gs
stepState _  _     gs@(GameState { gsPhase = Victory      }) = gs
stepState _  _     gs@(GameState { gsPhase = LevelCleared }) = gs
stepState _  _     gs@(GameState { gsPhase = WaitingToLaunch }) = gs
stepState dt input gs = stepPlaying dt input gs

stepPlaying :: Float -> Input -> GameState -> GameState
stepPlaying dt input gs =
    let paddle' = movePaddle dt input (gsPaddle gs)
        ball0   = gsBall gs
        ball1   = moveBall dt ball0
        ball2   = bouncePaddleWalls paddle' ball1
        (ball3, bricks', scored) = bounceBricks ball2 (gsBricks gs)
        score'  = gsScore gs + scored
    in  checkGameEnd $ gs
            { gsPaddle = paddle'
            , gsBall   = ball3
            , gsBricks = bricks'
            , gsScore  = score'
            , gsPhase  = Playing
            }
movePaddle :: Float -> Input -> Paddle -> Paddle
movePaddle dt input p =
    let dx    = (if inputLeft input then -paddleSpeed else 0.0)
              + (if inputRight input then  paddleSpeed else 0.0)
        newX  = clampPaddleX (paddleX p + dx * dt)
    in  p { paddleX = newX }

clampPaddleX :: Float -> Float
clampPaddleX x = max minX (min maxX x)
  where
    minX = -wallLeft + paddleHalfW
    maxX =  wallLeft - paddleHalfW
moveBall :: Float -> Ball -> Ball
moveBall dt b = b
    { ballX = ballX b + ballVx b * dt
    , ballY = ballY b + ballVy b * dt
    }
bouncePaddleWalls :: Paddle -> Ball -> Ball
bouncePaddleWalls paddle b =
    let b1 = bounceWalls b
        b2 = bouncePaddle paddle b1
    in  b2

bounceWalls :: Ball -> Ball
bounceWalls b = b
    { ballVx = if ballX b - ballRadius < -wallLeft
                    || ballX b + ballRadius >  wallLeft
               then -(ballVx b)
               else   ballVx b
    , ballVy = if ballY b + ballRadius > wallTop
               then -(ballVy b)
               else   ballVy b
    , ballX  = clamp (-(wallLeft - ballRadius)) (wallLeft - ballRadius) (ballX b)
    }
  where
    clamp lo hi v = max lo (min hi v)

bouncePaddle :: Paddle -> Ball -> Ball
bouncePaddle paddle b
    | collidesPaddle paddle b && ballVy b < 0 =
        let offset  = (ballX b - paddleX paddle) / paddleHalfW
            newVx   = ballInitialSpeed * offset * 0.8
            newVy   = abs (ballVy b)
        in  b { ballVx = newVx, ballVy = newVy }
    | otherwise = b

collidesPaddle :: Paddle -> Ball -> Bool
collidesPaddle paddle b =
    let bx = ballX b
        by = ballY b
        px = paddleX paddle
        py = paddleY
    in  bx >= px - paddleHalfW - ballRadius
     && bx <= px + paddleHalfW + ballRadius
     && by - ballRadius <= py + paddleHeight / 2.0
     && by + ballRadius >= py - paddleHeight / 2.0
bounceBricks :: Ball -> [Brick] -> (Ball, [Brick], Int)
bounceBricks ball bricks =
    foldl stepBrick (ball, [], 0) bricks
  where
    stepBrick (b, acc, sc) brick =
        case hitBrick b brick of
            Nothing         -> (b,  brick : acc, sc)
            Just (b', mBrick) ->
                let sc'  = sc + brickScore (brickHP brick)
                    acc' = maybe acc (: acc) mBrick
                in  (b', acc', sc')

hitBrick :: Ball -> Brick -> Maybe (Ball, Maybe Brick)
hitBrick b brick
    | not (overlaps b brick) = Nothing
    | otherwise =
        let b'    = reflectFromBrick b brick
            brick' = damageBrick brick
        in  Just (b', brick')

overlaps :: Ball -> Brick -> Bool
overlaps b brick =
    let nearX = clamp (brickX brick - brickWidth  / 2.0)
                      (brickX brick + brickWidth  / 2.0)
                      (ballX b)
        nearY = clamp (brickY brick - brickHeight / 2.0)
                      (brickY brick + brickHeight / 2.0)
                      (ballY b)
        dx    = ballX b - nearX
        dy    = ballY b - nearY
    in  dx * dx + dy * dy <= ballRadius * ballRadius
  where
    clamp lo hi v = max lo (min hi v)

reflectFromBrick :: Ball -> Brick -> Ball
reflectFromBrick b brick =
    let overlapX = (brickWidth  / 2.0 + ballRadius) - abs (ballX b - brickX brick)
        overlapY = (brickHeight / 2.0 + ballRadius) - abs (ballY b - brickY brick)
    in  if overlapX < overlapY
        then b { ballVx = -(ballVx b) }
        else b { ballVy = -(ballVy b) }

damageBrick :: Brick -> Maybe Brick
damageBrick brick = case brickHP brick of
    One   -> Nothing
    Two   -> Just brick { brickHP = One   }
    Three -> Just brick { brickHP = Two   }

brickScore :: BrickHP -> Int
brickScore One   = 10
brickScore Two   = 20
brickScore Three = 30
checkGameEnd :: GameState -> GameState
checkGameEnd gs
    | null (gsBricks gs)          = gs { gsPhase = Victory }
    | ballY (gsBall gs) < -wallTop = loseLife gs
    | otherwise                    = gs

loseLife :: GameState -> GameState
loseLife gs
    | gsLives gs <= 1 = gs { gsLives = 0, gsPhase = GameOver }
    | otherwise =
        let lives'  = gsLives gs - 1
            paddle' = Paddle 0.0
            ball'   = ballOnPaddle paddle'
        in  gs
                { gsLives  = lives'
                , gsPaddle = paddle'
                , gsBall   = ball'
                , gsPhase  = WaitingToLaunch
                }

module Render (renderWorld) where

import Graphics.Gloss

import Types
import Constants
colorBackground :: Color
colorBackground = makeColorI 15 15 30 255

colorPaddle :: Color
colorPaddle = makeColorI 100 200 255 255

colorBall :: Color
colorBall = makeColorI 255 255 255 255

colorWall :: Color
colorWall = makeColorI 40 40 80 255

colorHUD :: Color
colorHUD = makeColorI 180 180 220 255

colorOverlay :: Color
colorOverlay = makeColorI 0 0 0 180

colorTitle :: Color
colorTitle = makeColorI 255 220 80 255

brickColor :: BrickHP -> Color
brickColor Three = makeColorI 255  80  80 255
brickColor Two   = makeColorI 255 160  40 255
brickColor One   = makeColorI  80 200  80 255
renderWorld :: World -> Picture
renderWorld w = renderState (worldState w)

renderState :: GameState -> Picture
renderState gs =
    Pictures
        [ drawBackground
        , drawWalls
        , drawBricks (gsBricks gs)
        , drawPaddle (gsPaddle gs)
        , drawBall   (gsBall   gs) (gsPhase gs)
        , drawHUD    (gsLives  gs) (gsScore  gs)
        , drawOverlay (gsPhase gs) (gsScore gs)
        ]
drawBackground :: Picture
drawBackground =
    Color colorBackground $
    rectangleSolid
        (fromIntegral windowWidth)
        (fromIntegral windowHeight)

drawWalls :: Picture
drawWalls =
    Color colorWall $ Pictures
        [ Translate (-wallLeft) 0
            $ rectangleSolid wallThick (fromIntegral windowHeight)
        , Translate ( wallLeft) 0
            $ rectangleSolid wallThick (fromIntegral windowHeight)
        , Translate 0 wallTop
            $ rectangleSolid (fromIntegral windowWidth) wallThick
        ]
  where
    wallThick :: Float
    wallThick = 6.0
drawBricks :: [Brick] -> Picture
drawBricks = Pictures . map drawBrick

drawBrick :: Brick -> Picture
drawBrick b =
    Translate (brickX b) (brickY b) $
    Pictures
        [ Color (brickColor (brickHP b))
            $ rectangleSolid (brickWidth - 2.0) (brickHeight - 2.0)
        , Color (withAlpha 0.3 white)
            $ rectangleWire (brickWidth - 2.0) (brickHeight - 2.0)
        ]
drawPaddle :: Paddle -> Picture
drawPaddle p =
    Translate (paddleX p) paddleY $
    Pictures
        [ Color colorPaddle
            $ rectangleSolid paddleWidth paddleHeight
        , Color (withAlpha 0.5 white)
            $ rectangleWire paddleWidth paddleHeight
        ]
drawBall :: Ball -> Phase -> Picture
drawBall b phase =
    let alpha = case phase of
                    WaitingToLaunch -> 0.6
                    _               -> 1.0
    in  Translate (ballX b) (ballY b) $
        Pictures
            [ Color (withAlpha alpha colorBall)
                $ circleSolid ballRadius
            , Color (withAlpha (alpha * 0.4) white)
                $ circle ballRadius
            ]
drawHUD :: Int -> Int -> Picture
drawHUD lives score =
    Pictures
        [ Translate (-wallLeft + 10.0) (-wallTop + 20.0)
            $ Color colorHUD
            $ Scale 0.15 0.15
            $ Text ("Lives: " ++ show lives)
        , Translate (wallLeft - 140.0) (-wallTop + 20.0)
            $ Color colorHUD
            $ Scale 0.15 0.15
            $ Text ("Score: " ++ show score)
        ]
drawOverlay :: Phase -> Int -> Picture
drawOverlay Playing         _ = Blank
drawOverlay WaitingToLaunch _ =
    overlayBox
        [ centeredText 0.20 colorTitle   40.0  "ARKANOID"
        , centeredText 0.13 colorHUD    (-10.0) "Press SPACE to launch"
        , centeredText 0.11 colorHUD    (-50.0) "Arrow keys to move paddle"
        ]
drawOverlay LevelCleared    _ =
    overlayBox
        [ centeredText 0.20 colorTitle   30.0  "LEVEL CLEARED!"
        , centeredText 0.13 colorHUD    (-30.0) "Press SPACE to continue"
        ]
drawOverlay Victory         sc =
    overlayBox
        [ centeredText 0.22 colorTitle   40.0  "YOU WIN!"
        , centeredText 0.13 colorHUD    (-10.0) ("Final score: " ++ show sc)
        , centeredText 0.11 colorHUD    (-50.0) "Press SPACE to restart"
        ]
drawOverlay GameOver        sc =
    overlayBox
        [ centeredText 0.22 (makeColorI 255 60 60 255)  40.0  "GAME OVER"
        , centeredText 0.13 colorHUD                   (-10.0) ("Score: " ++ show sc)
        , centeredText 0.11 colorHUD                   (-50.0) "Press SPACE to restart"
        ]

overlayBox :: [Picture] -> Picture
overlayBox ps =
    Pictures
        [ Color colorOverlay
            $ rectangleSolid (fromIntegral windowWidth) 180.0
        , Pictures ps
        ]

centeredText :: Float -> Color -> Float -> String -> Picture
centeredText sc col yOff str =
    Translate (-textHalfWidth) yOff $
    Color col $
    Scale sc sc $
    Text str
  where
    charW         :: Float
    charW         = 104.0
    textHalfWidth = fromIntegral (length str) * charW * sc / 2.0
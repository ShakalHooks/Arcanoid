module Main where

import Graphics.Gloss

import Constants
import Logic
import Render

window :: Display
window = InWindow windowTitle (windowWidth, windowHeight) (100, 100)

main :: IO ()
main =
    play
        window
        black
        fps
        initialWorld
        renderWorld
        handleEvent
        stepWorld
module Main where

import Graphics.Gloss (simulate, Display (FullScreen), black)
import World (testWorld, step, render)

main :: IO ()
main = simulate
  FullScreen
  black
  60
  testWorld
  render
  step

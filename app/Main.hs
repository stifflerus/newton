module Main where

import Graphics.Gloss
import World
import Vector
import Object

main :: IO ()
main = simulate
       (InWindow "Simulation" (200, 200) (10, 10))
       white
       1000
       testWorld
       render
       step

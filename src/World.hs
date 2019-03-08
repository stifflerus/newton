module World where

import Object
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import Vector

data World = World [Object]

--render the world to a picture
render :: World -> Picture
render (World os) = pictures $ fmap renderObject os

-- advance the state of the world
step :: ViewPort -> Float -> World -> World
step _ f (World os) = World (fmap (stepObject f) os)

-- world with nothing in it
emptyWorld :: World
emptyWorld = World []

-- world with some stuff in it for testing
testWorld :: World
testWorld = emptyWorld
          +@+ Object zeroV (Vector 10 10)
          +@+ Object (Vector (-10.0) 0) (Vector (-5) 10)
          +@+ Object zeroV zeroV

-- adds object to the world
(+@+) :: World -> Object -> World
(World os) +@+ o = World (o:os)

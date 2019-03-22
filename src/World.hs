module World where

import Object (Object, stepObject, renderObject)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate
import Vector (zeroV)

type World = [Object]

--render the world to a picture
render :: World -> Picture
render os = pictures $ fmap renderObject os

-- advance the state of the world
step :: ViewPort -> Float -> World -> World
step _ f os = fmap (stepObject f os) os

-- world with nothing in it
emptyWorld :: World
emptyWorld = []

-- world with some stuff in it for testing
testWorld :: World
testWorld = emptyWorld
          +@+ (zeroV, zeroV)
          +@+ ((10, 10), (10, 10))
          +@+ ((15, 15), (-15, 0))
          +@+ ((26, 15), (-15, 0))
          +@+ ((37, 15), (-15, 0))
          +@+ ((48, 15), (-15, 0))
          +@+ ((59, 15), (-15, 0))
          +@+ ((61, 15), (-15, 0))

-- adds object to the world
(+@+) :: World -> Object -> World
os +@+ o = o:os

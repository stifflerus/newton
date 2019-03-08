module Object where

import Vector
import Graphics.Gloss (Picture, translate, circle)

data Object = Object { position :: Vector, velocity :: Vector }

-- Render the object into a picture
renderObject :: Object -> Picture
renderObject (Object p _) = translate (x p) (y p) (circle 10)

stepObject :: Float -> Object -> Object
stepObject t (Object p v) = Object p' v
  where p' = p ^+^ d
        d  = t *^ v

module Object where

import Vector (Vector, vdistance, (*^), (^+^), (^-^), x, y, normalize)
import Graphics.Gloss (Picture, translate, circle)

type Object = (Vector, Vector)

-- get the position of an object
position :: Object -> Vector
position = fst

-- get the veloctiy of an object
velocity :: Object -> Vector
velocity = snd

-- Render the object into a picture
renderObject :: Object -> Picture
renderObject (p, _) = translate (x p) (y p) (circle 10)

-- Calculate the new position and speed of the object
stepObject :: Float -> [Object] -> Object -> Object
stepObject dt os o = (p', v')
  where p  = position o
        v  = velocity o
        p' = p ^+^ dp
        dp = dt *^ v
        v' = v ^+^ dv
        dv = dt *^ a
        a  = foldr1 (^+^) (fmap (gravity o) os)

-- distance between two objects
distance :: Object -> Object -> Float
distance o1 o2 = vdistance (position o1) (position o2)

-- acceleration due to gravity on the first object
gravity :: Object -> Object -> Vector
gravity o1 o2 = a *^ u
  where a = acceleration o1 o2
        u = normalize d
        d = (position o2) ^-^ (position o1)

-- magnitude of acceleration between two objects
acceleration :: Object -> Object -> Float
acceleration o1 o2 = 1 / dsquared
  where dsquared = (distance o1 o2)**2

module Object where

import Vector (Vector, vdistance, zeroV, (*^), (^+^), (^-^), x, y, normalize)
import Graphics.Gloss (Picture, pictures, translate, circle, line, color, white, red)

type Object = (Vector, Vector)

-- get the position of an object
position :: Object -> Vector
position = fst

-- get the veloctiy of an object
velocity :: Object -> Vector
velocity = snd

-- Render the object into a picture
renderObject :: Object -> Picture
renderObject (p, v) = pictures [outline, velocity]
  where
    outline  = color white $ translate (x p) (y p) (circle 10)
    velocity = color red $ (line [p, p ^+^ v])

-- Calculate the new position and speed of the object
stepObject :: Float -> [Object] -> Object -> Object
stepObject dt w o = (p', v')
  where p  = position o
        p' = p ^+^ dp
        dp = dt *^ v
        v  = velocity o
        v' = v ^+^ dv
        dv = dt *^ a
        a  = foldr (^+^) zeroV (fmap (gravity o) (dropA w o))

-- remove item from list
dropA :: (Eq a) => [a] -> a -> [a]
dropA [] a = []
dropA (x:xs) a
  | x == a    = dropA xs a
  | otherwise = x:(dropA xs a)

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
acceleration o1 o2 = 1000 / dsquared
  where dsquared = (distance o1 o2)**2

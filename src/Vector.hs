module Vector where

type Vector = (Float, Float)

x :: Vector -> Float
x = fst

y :: Vector -> Float
y = snd

vmap :: (Float -> Float) -> Vector -> Vector
vmap f (x, y) = ((f x), (f y))

vzip :: (Float -> Float -> Float) -> Vector -> Vector -> Vector
vzip f (x1, y1) (x2, y2) = ((f x1 x2), (f y1 y2))

vnegate :: Vector -> Vector
vnegate = vmap negate

normalize :: Vector -> Vector
normalize v = v ^/ (magnitude v)

-- Vector addition
(^+^) :: Vector -> Vector -> Vector
(^+^) = vzip (+)

-- Vector subtraction
(^-^) :: Vector -> Vector -> Vector
(^-^) = vzip (-)

-- Scalar multiplication
(*^) :: Float -> Vector -> Vector
f *^ v = vmap (*f) v

-- Scalar multiplication
(^*) :: Vector -> Float -> Vector
(^*) = flip (*^)

--scalar division
(^/) :: Vector -> Float -> Vector
v ^/ f = vmap (/f) v

-- magnitude of the Vector
magnitude :: Vector -> Float
magnitude v = sqrt ((x v)**2 + (y v)**2)

-- zero vector
zeroV :: Vector
zeroV = (0.0, 0.0)

-- distance between two points
vdistance :: Vector -> Vector -> Float
vdistance v1 v2 = sqrt (xdiffsquared + ydiffsquared)
  where diffSquared f = ((f v1) - (f v2))**2
        xdiffsquared = diffSquared x
        ydiffsquared = diffSquared y

module Vector where

data Vector = Vector { x :: Float, y :: Float }

vmap :: (Float -> Float) -> Vector -> Vector
vmap f (Vector x y) = Vector (f x) (f y)

vzip :: (Float -> Float -> Float) -> Vector -> Vector -> Vector
vzip f (Vector x1 y1) (Vector x2 y2) = Vector (f x1 x2) (f y1 y2)

-- Vector addition
(^+^) :: Vector -> Vector -> Vector
(^+^) = vzip (+)

-- Vector subtraction
(^-^) :: Vector -> Vector -> Vector
(^-^) = vzip (+)

-- Scalar multiplication
(*^) :: Float -> Vector -> Vector
f *^ v = vmap (*f) v

-- Scalar multiplication
(^*) :: Vector -> Float -> Vector
v ^* f = vmap (*f) v

--scalar division
(^/) :: Vector -> Float -> Vector
v ^/ f = vmap (/f) v

-- zero vector
zeroV :: Vector
zeroV = Vector 0.0 0.0

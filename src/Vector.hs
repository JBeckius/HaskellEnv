module Vector
    (
        Vector(..),
        vplus,
        dotProd,
        vmult,
        Vector2d(..),
        vplus2d,
        dotProd2d,
        vmult2d,
        vdir2d,
        vmag2d

    ) where

data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i + l) (j + m) (k + n)

dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

data Vector2d a = Vector2d a a deriving (Show)

vplus2d :: (Num a) => Vector2d a -> Vector2d a -> Vector2d a
(Vector2d j k) `vplus2d` (Vector2d l m) = Vector2d (j + l) (k + m)

dotProd2d :: (Num a) => Vector2d a -> Vector2d a -> a
(Vector2d j k) `dotProd2d` (Vector2d l m) = j*l + k*m

vmult2d :: (Num a) => Vector2d a -> a -> Vector2d a
(Vector2d j k) `vmult2d` m = Vector2d (j*m) (k*m)

vdir2d :: (Floating a) => Vector2d a -> a
vdir2d (Vector2d x y) = atan (y / x) * (180 / pi)

vmag2d :: (Floating a) => Vector2d a -> a
vmag2d (Vector2d x y) = sqrt (x^2 + y^2)
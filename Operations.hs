module Operations (
    (•), (••), (.-), (.*), (^*),
    transpose
) where

import Prelude hiding (zipWith, sum, map, length)
import Data.Vector.Generic (Vector, 
    zipWith, sum, map, convert, generate, length, (!))

{-# INLINABLE (•) #-}
(•) :: (Vector v a, Num a) => v a -> v a -> a
a • b = sum (zipWith (*) a b)

{-# INLINABLE (••) #-}
(••) :: (Vector v1 (v2 a), Vector v1 a, Vector v2 a, Num a) => v2 a -> v1 (v2 a) -> v2 a
a •• b = convert (map (a •) b)

(.-) :: (Vector v a, Num a) => v a -> v a -> v a
a .- b = zipWith (-) a b

(.*) :: (Vector v a, Num a) => v a -> v a -> v a
a .* b = zipWith (*) a b

(^*) :: (Vector v a, Vector w (v a), Vector w a, Num a) => v a -> v a -> w (v a)
a ^* b = map (\x -> map (*x) b) (convert a)

transpose :: (Vector v1 (v2 a), Vector v2 a) => v1 (v2 a) -> v1 (v2 a)
transpose v = generate width $ \x -> generate height $ \y -> (v ! y) ! x
    where
    height = length v
    width  = length (v ! 0)
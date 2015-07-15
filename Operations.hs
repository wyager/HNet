module Operations (
	(•), (••)
) where

import Prelude hiding (zipWith, sum, map)
import Data.Vector.Generic (Vector, zipWith, sum, map, convert)

(•) :: (Vector v a, Num a) => v a -> v a -> a
a • b = sum (zipWith (*) a b)

(••) :: (Vector v1 (v2 a), Vector v1 a, Vector v2 a, Num a) => v2 a -> v1 (v2 a) -> v2 a
a •• b = convert (map (a •) b)
	--where a • b = sum (zipWith (*) a b)

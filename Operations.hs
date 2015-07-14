module Operations (
	(•), (••)
) where

import Prelude hiding (zipWith, sum, length, map)
import Data.Vector.Generic (Vector, (!), zipWith, sum, length, generate, map, convert)
import GHC.Magic (inline)

(•) :: (Vector v a, Num a) => v a -> v a -> a
a • b = sum (zipWith (*) a b)

-- 0.05! wtf!
(••) :: (Vector v1 (v2 a), Vector v1 a, Vector v2 a, Num a) => v2 a -> v1 (v2 a) -> v2 a
a •• b = convert (map (a •) b)
	--where a • b = sum (zipWith (*) a b)

-- Why are these so much slower?
-- 1.25
--(••) :: (Vector v1 (v2 a), Vector v2 a, Num a) => v2 a -> v1 (v2 a) -> v2 a
--a •• b = unstream . fmap (a •) . stream $ b

-- 0.88
--(••) :: (Vector v1 (v2 a), Vector v2 a, Num a) => v2 a -> v1 (v2 a) -> v2 a
--a •• b = generate (length b) (\i -> a • (b ! i))
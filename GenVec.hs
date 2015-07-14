import Data.Vector.Generic (Vector, (!))

f :: Vector v a => v a -> a
f = (! 0)

main = undefined

module Input (
	Input, Activation
) where

import Data.Vector.Unboxed (Vector)

type Activation = Double

type Input = Vector Activation

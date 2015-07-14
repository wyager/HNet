{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Weight (
	Weight
) where

import Data.Vector.Unboxed (Unbox, Vector, MVector)
import qualified Data.Vector.Generic as G 
import qualified Data.Vector.Generic.Mutable as GM

newtype Weight = Weight Double deriving (Unbox, G.Vector Vector, GM.MVector MVector)
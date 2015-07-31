{-# LANGUAGE FlexibleContexts #-}

module Neuron (
    Weight, Neuron, Layer, Network, Input
) where

import Data.Vector.Unboxed as U (Vector)
import Data.Vector as V (Vector)
import Data.Vector.Generic as G (Vector)

type Neuron  w = U.Vector w
type Layer   w = V.Vector (Neuron w)
type Network w = V.Vector (Layer w)

type Input a = U.Vector a

class (G.Vector U.Vector a, Floating a) => Weight a

instance Weight Double 
instance Weight Float
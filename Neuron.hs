module Neuron (
	Neuron, Weight
) where

import Data.Vector.Unboxed (Vector)

type Weight = Double

type Neuron = Vector Weight

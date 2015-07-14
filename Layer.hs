module Layer (
	Layer
) where

import Neuron (Neuron)
import Data.Vector (Vector)

type Layer = Vector Neuron

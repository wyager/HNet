module Network (
	Network
) where

import Data.Vector (Vector)
import Layer (Layer)

type Network = Vector Layer

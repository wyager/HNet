{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (map, replicate, sum)
import Operations ((••), (.-), (.*))
import Neuron (Weight, Neuron, Layer, Network, Input)
import Data.Vector (Vector)
import Data.Vector.Generic (fromList, map, foldl', scanl', replicate, sum)

network :: Weight a => Network a
network = replicate 50 $ replicate 10 $ replicate 10 $ 0.99

-- Runs the input through the NN
evaluate :: Weight a => Input a -> Network a -> Input a
evaluate = foldl' apply

-- Runs the input through the NN and saves intermediate outputs
evaluateAll :: Weight a => Input a -> Network a -> Vector (Input a)
evaluateAll = scanl' apply

apply :: Weight a => Input a -> Layer a -> Input a
apply input layer = map sigmoid (input •• layer)
{-# SPECIALIZE (••) :: Input Double -> Layer Double -> Input Double #-}

sigmoid :: Floating a => a -> a
sigmoid x = 1.0 / (1.0 + exp (negate x))

differentiate :: Weight a => Input a -> Input a
differentiate = map (\n -> n * (1.0 - n))

inputs :: Double -> Vector (Input Double)
inputs max = fromList $ [replicate 10 x | x <- [0.0, 0.01 .. max]]

foo max = sum $  map (sum . (`evaluate` network)) (inputs max)

main = do
    print $ foo 500.0

--train :: Weight a => Network a -> (Input a, Input a) -> Network a
--train network (input, output) = 
--	where
--	results = reverse (evaluateAll input network)
--	result = head results
--	error = output .- result :: Input a
--	derr_din = derr_dout .* dout_din :: Input a
--	derr_dout = error :: Input a
--	dout_din  = differentiate result :: Input a
--	-- Outer axis indexed by output neuron, inner axis indexed by input neuron?
--	derr_dweights = derr_din ^* din_dweights :: Layer a 
--	din_dweights = result' :: Input a
--	result' = head (tail results) :: Input a

--	derr_dout' = result' •• derr_dweights :: Input a -- There are probably other ways of finding this
--	derr_din' = derr_dout' .* dout'_din'
--	dout'_din' = differentiate result'
--	derr_dweights' = derr_din' *? din'_dweights'
--	din'_dweights' = result''

-- Todo: Check that the derr_dout' definition is valid
-- Todo: Check that I'm using the ^* and •• here correctly


--[[Weight]] -> [[Weight]]
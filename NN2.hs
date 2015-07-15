import Prelude hiding (map, replicate, sum)
import Operations ((•), (••))
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

sigmoid :: Floating a => a -> a
sigmoid x = 1.0 / (1.0 + exp (negate x))

inputs :: Double -> Vector (Input Double)
inputs max = fromList $ [replicate 10 x | x <- [0.0, 0.01 .. max]]

foo max = sum $  map (sum . (`evaluate` network)) (inputs max)

main = do
 	print $ foo 500.0

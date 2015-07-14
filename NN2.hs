import Prelude hiding (map, replicate, sum)
import Operations ((•), (••))
import Neuron (Neuron, Weight)
import Layer (Layer)
import Network (Network)
import Input (Input, Activation)
import Data.Vector (Vector)
import Data.Vector.Generic (fromList, map, foldl', scanl', replicate, sum)

thing :: Neuron
thing = fromList [1,2,3]

network :: Network
network = replicate 50 $ replicate 10 $ replicate 10 $ 0.99

-- Runs the input through the NN
evaluate :: Input -> Network -> Input
evaluate = foldl' apply

-- Runs the input through the NN and saves intermediate outputs
evaluateAll :: Input -> Network -> Vector Input
evaluateAll = scanl' apply

apply :: Input -> Layer -> Input
apply input layer = map sigmoid (input •• layer)

sigmoid :: Activation -> Activation
sigmoid x = 1.0 / (1.0 + exp (negate x))

inputs :: Double -> Vector Input
inputs max = fromList $ [replicate 10 x | x <- [0.0, 0.01 .. max]]

foo max = sum $  map (sum . (`evaluate` network)) (inputs max)

main = do
 	print $ foo 500.0
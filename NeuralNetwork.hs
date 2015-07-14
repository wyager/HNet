{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (sum, zipWith, length)
import Data.Vector (Vector, fromList, sum, (!), zipWith, length, generate)
import qualified Data.Vector as V
import Data.Word (Word)

newtype Neuron  a = Neuron  { weightsOf :: Vector a } deriving Show
newtype Layer   a = Layer   { neuronsOf :: Vector (Neuron a) } deriving Show
newtype Network a = Network { layersOf ::  Vector (Layer a) } deriving Show

newtype Activation a = Activation { valuesOf :: Vector a } deriving Show
newtype Activations a = Activations { activationsOf :: Vector (Activation a) } deriving Show

newtype LayerDeltas a = LayerDeltas {deltasOf :: Vector a} deriving Show
newtype NetDeltas a = NetDeltas {netDeltasOf :: Vector (LayerDeltas a) } deriving Show



biasOf :: Neuron a -> a
biasOf neuron = (weightsOf neuron) ! 0

makeNeuron :: [a] -> Neuron a
makeNeuron = Neuron . fromList

makeLayer :: [[a]] -> Layer a
makeLayer = Layer . fromList . map makeNeuron

makeNetwork :: [[[a]]] -> Network a
makeNetwork = Network . fromList . map makeLayer

makeZeros :: Num a => [Word] -> Network a
makeZeros sizes = makeNetwork [[[0 | i <- [0..input]] 
                                   | w <- [1..width]] 
                                   | (input, width) <- zip sizes (tail sizes)]

main = print $ makeNetwork [[[1 :: Int]]]


--classify :: Floating a => Vector a -> Network a -> Vector a
--classify input network = classify' input 0 network
--classify' :: Floating a => Vector a -> Int -> Network a -> Vector a
--classify' input n (Network layers)
--    | n == length layers = input
--    | otherwise          = classify' output (n+1) (Network layers)
--    where
--    output = V.map (σ . compute) neurons
--    compute neuron = biasOf neuron + multiply neuron
--    multiply neuron = dot input . V.tail . weightsOf $ neuron
--    neurons = neuronsOf (layers ! n)

σ :: Floating a => a -> a
σ x = 1 / (1 + exp (negate x))

train :: Network a -> [(Vector a, Vector a)] -> Network a
train = undefined

activate :: forall a . Floating a => Network a -> Activation a -> Activations a
activate (Network layers) input = Activations $ activations
    where
    neuron x y = neuronsOf (layers ! x) ! y
    activations = generate (length layers) (Activation . activateLayer)
    activateLayer x = generate (length $ neuronsOf (layers ! x)) (activateNeuron x)
    activateNeuron 0 y = compute (neuron 0 y) $ input
    activateNeuron x y = compute (neuron x y) $ activations ! (x-1)

--dot :: Num a => Vector a -> Vector a
--dot a b
--    | length a /= length b = error "Dotting mismatched lengths"
--    | otherwise = sum $ zipWith (*) a b

-- dCost/dInput for all neurons
--deltas :: forall a . Network a -> Activation a -> Activations a -> NetDeltas a
--deltas network (Activation ideal) (Activations actual) = NetDeltas $ reverse netDeltas
--    where
--    layer :: Int -> Activation a
--    layer x = valuesOf (actual ! x)
--    netDeltas :: Vector (LayerDeltas a)
--    netDeltas = generate (length actual) (LayerDeltas . layerDeltas)
--    layerDeltas :: Int -> Vector a
--    layerDeltas x = generate (length (layer x)) (neuronDelta derivatives x)
--        derivatives :: Vector a
--        where derivatives = zipWith (*) (layer x) $ map (1-) (layer x)
--    neuronDelta :: Vector a -> Int -> Int -> a
--    neuronDelta derivatives x y 
--        | x == length actual - 1 = dot derivatives (zipWith (-) (layer x) ideal)
--        | otherwise = (derivatives ! y) * sum valueDerivatives
--        where 
--        valueDerivatives :: Vector a
--        valueDerivatives = generate (length $ layer (x+1)) valueDerivative
--        valueDerivative :: Int -> a -- The dCost/dInput of nth neuron in the next layer
--        valueDerivative n = delta (x+1) n * weight (x+1) n y
--        delta :: Int -> Int -> a
--        delta x y = deltasOf (netDeltas ! x) ! y
--        weight :: Int -> Int -> Int -> a
--        weight x y n = (!n) . weightsOf . (!y) . neuronsOf . (!x) . layersOf $ network


        

compute :: Floating a => Neuron a -> Activation a -> a
compute neuron (Activation input) = σ (biasOf neuron + total)
    where total = sum . zipWith (*) input . V.tail . weightsOf $ neuron

--consider :: Network a -> (Vector a, Vector a) -> Network a
--consider network (input, correct) = merge network dw 
--    where
--    dw = - α * costGradient
--    costGradient = backprop input network correct

--delta :: a -> a -> a -> a -> (a -> a) -> a -> a
--delta α target actual inputSum σ' input = α * (target - actual) * (σ' inputSum) * input

--deltas :: a -> Vector a -> Vector a -> Vector a -> (a -> a)
--deltas α target actual input σ' = 

--delta :: Num a => Vector a -> a -> a -> a -> Σ' a -> Vector a
--delta inputs α target actual σ' = V.map (* adjustment) inputs
--    where adjustment = α * (target - actual) * σ' (sum inputs)


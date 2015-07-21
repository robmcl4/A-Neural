module Brain
(
  NetworkWithSamples (..)
, constructNeuralNetwork
, trainN
, activate
)

where

--------------------------------------------------------------------------------

import AI.HNN.FF.Network          ( Network
                                  , Samples
                                  , createNetwork
                                  , trainNTimes
                                  , tanh
                                  , tanh'
                                  , output )
import Numeric.LinearAlgebra.Data ( Vector )
import Debug.Trace


--------------------------------------------------------------------------------

type Network' = Network Double
type Samples' = Samples Double

data NetworkWithSamples = NetworkWithSamples { network :: Network'
                                             , samples :: Samples' }
                          deriving (Show)


constructNeuralNetwork :: Int -> [Int] -> Int -> Samples' -> IO (NetworkWithSamples)
constructNeuralNetwork i h o s = do
    net <- createNetwork i h o
    return NetworkWithSamples { network = net, samples = s }


trainN :: NetworkWithSamples -> Int -> NetworkWithSamples
trainN nn n = nn { network = nn' }
  where nn' = trainNTimes n 0.05 tanh tanh' (network nn) (samples nn)


activate :: NetworkWithSamples -> Vector Double -> Vector Double
activate n s = output (network n) tanh s

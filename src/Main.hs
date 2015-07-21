import ImageLoader
import Brain

--------------------------------------------------------------------------------

import Control.Exception (SomeException (..))
import System.Random     (mkStdGen, randomR)
import Numeric.LinearAlgebra
import Data.List
import qualified Numeric.LinearAlgebra.Data as V
import Debug.Trace


--------------------------------------------------------------------------------

type Samples = [(V.Vector Double, V.Vector Double)]


main :: IO ()
main = loadFiles


loadFiles :: IO ()
loadFiles = do
    ex    <- loadExamples
    notEx <- loadNonExamples
    case (ex, notEx)
      of (Left (SomeException e), _) -> printError e
         (_, Left (SomeException e)) -> printError e
         (Right v1, Right v2)        -> runTraining v1 v2
  where
    printError e = putStrLn $ "Exception: " ++ (show e)


runTraining :: Samples -> Samples -> IO ()
runTraining s1 s2 = do
    net <- constructNeuralNetwork 256 [512, 256, 128, 64] 1 all_
    putStrLn "----"
    printSamples all_ net
    putStrLn "----"
    let net' = trainN net 800
    printSamples all_ net'
  where all_   = s1 ++ s2


printSamples :: Samples -> NetworkWithSamples -> IO ()
printSamples [] _       = return ()
printSamples (x:xs) net = do
    putStrLn $ vid ++ " --> " ++ output ++ " ? " ++ expectedOut
    printSamples xs net
  where in_    = fst x
        expectedOut = show $ snd x
        vid    = show $ (vecId in_)
        output = show $ (activate net in_)


-- Given a list of examples, separates into a list of
-- test data (fst) and training data (snd)
sepExTest :: [a] -> ([a], [a])
sepExTest xs = go xs ([], []) (mkStdGen 10)
  where
        go [] a _            = a
        go (v:vs) (a, b) gen = case randomR (0, 99) gen
                               of (n, gen') -> if n < (20 :: Int)
                                                  then go vs (v:a, b) gen'
                                                  else go vs (a, v:b) gen'

vecId :: V.Vector Double -> Double
vecId v = v <.> V.fromList (replicate 256 (1 :: Double))

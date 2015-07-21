{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}


module ImageLoader
(
  loadExamples
, loadNonExamples
)

where

--------------------------------------------------------------------------------


import Control.Exception          ( Exception
                                  , try )
import Control.Monad              ( sequence )
import System.FilePath            ( FilePath
                                  , (</>) )
import Vision.Image.Type          ( manifestVector )
import Vision.Image.RGB.Type      ( RGB )
import Vision.Image.Grey.Type     ( Grey )
import Vision.Image.Storage.DevIL ( Autodetect (..)
                                  , load )
import Paths_img_neural           ( getDataFileName )


import qualified Data.Vector.Storable       as VS
import qualified Numeric.LinearAlgebra.Data as VD

--------------------------------------------------------------------------------

type Samples = [(VD.Vector Double, VD.Vector Double)]


-- Load the vectors which represent an example of the letter A
loadExamples :: Exception e => IO (Either e Samples)
loadExamples = try $ do
    images <- loadImages exampleImageNames
    return $ map (\x -> (x, output)) images
  where output = VD.fromList [1 :: Double]


-- Load the vectors which do not represent an example of the letter A
loadNonExamples :: Exception e => IO (Either e Samples)
loadNonExamples = try $ do
    images <- loadImages nonExampleImageNames
    return $ map (\x -> (x, output)) images
  where output = VD.fromList [0 :: Double]

--------------------------------------------------------------------------------

-- Lists all images that are examples of the letter A
exampleImageNames :: [FilePath]
exampleImageNames = map ("A" </>) [
                      "A1.png"
                    , "A2.png"
                    , "A3.png"
                    , "A4.png"
                    , "A5.png"
                    , "A6.png" ]


-- Lists all images that are _not_ examples of the letter A
nonExampleImageNames :: [FilePath]
nonExampleImageNames = map ("not-A" </>) [
                         "B1.png"
                       , "B2.png"
                       , "B3.png"
                       , "B4.png"
                       , "B5.png"
                       , "B6.png"
                       , "B7.png" ]


-- Loads images from the given list
loadImages :: [FilePath] -> IO [VD.Vector Double]
loadImages fp = do
    fnames  <- map' getFileName fp
    vectors <- map' loadFile fnames
    return $ map convertToLinearVector vectors
  where map' f xs   = sequence $ map f xs
        getFileName = getDataFileName . ("res" </>)


-- Loads the given file to an image, converted to a Vector
loadFile :: FilePath -> IO (VS.Vector Double)
loadFile f = do
    dat <- load Autodetect f
    case dat
      of Left  e           -> error  $ show e
         Right (i :: Grey) -> return $ greyManifestToVector i


-- Converts a Grey into a Vector Int
greyManifestToVector :: Grey -> VS.Vector Double
greyManifestToVector m = VS.map (\i -> 1 - (fromIntegral $ round $ (fromIntegral i)/256)) (manifestVector m)


-- Converts a Data/Vector to a LinearAlgebra/Vector
convertToLinearVector :: VS.Storable a => VS.Vector a -> VD.Vector a
convertToLinearVector = VD.fromList . VS.toList

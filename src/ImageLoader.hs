{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}


module ImageLoader
(
  loadExamples
, loadNonExamples
)

where

--------------------------------------------------------------------------------

import Control.Exception
import Control.Monad (sequence)
import Debug.Trace
import System.FilePath
import Vision.Image.Type (manifestVector)
import Vision.Image.RGB.Type (RGB)
import Vision.Image.Grey.Type (Grey)
import Vision.Image.Storage.DevIL
import Paths_img_neural
import qualified Data.Vector.Storable as V

--------------------------------------------------------------------------------

-- Load the vectors which represent an example of the letter A
loadExamples :: Exception e => IO (Either e [V.Vector Int])
loadExamples = try $ loadImages $ map ("A" </>) exampleImageNames


-- Load the vectors which do not represent an example of the letter A
loadNonExamples :: Exception e => IO (Either e [V.Vector Int])
loadNonExamples = try $ loadImages $ map ("not-A" </>) nonExampleImageNames

--------------------------------------------------------------------------------

-- Lists all images that are examples of the letter A
exampleImageNames :: [FilePath]
exampleImageNames = [ "A1.png"
                    , "A2.png"
                    , "A3.png"
                    , "A4.png"
                    , "A5.png" ]


-- Lists all images that are _not_ examples of the letter A
nonExampleImageNames :: [FilePath]
nonExampleImageNames = [ "B1.png"
                       , "B2.png"
                       , "B3.png"
                       , "B4.png"
                       , "C1.png" ]


-- Loads images from the given list
loadImages :: [FilePath] -> IO [V.Vector Int]
loadImages fp = do
    fnames <- map' getFileName fp
    map' loadFile fnames
  where map' f xs   = sequence $ map f xs
        getFileName = getDataFileName . ("res" </>)


-- Loads the given file to an image, converted to a Vector
loadFile :: FilePath -> IO (V.Vector Int)
loadFile f = do
    dat <- load Autodetect f
    case dat
      of Left  e           -> error  $ show e
         Right (i :: Grey) -> return $ greyManifestToVector i


-- Converts a Grey into a Vector Int
greyManifestToVector :: Grey -> V.Vector Int
greyManifestToVector m = V.map (fromIntegral) (manifestVector m)
